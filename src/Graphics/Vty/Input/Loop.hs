{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- | The input layer used to be a single function that correctly
-- accounted for the non-threaded runtime by emulating the terminal
-- VMIN adn VTIME handling. This has been removed and replace with a
-- more straightforward parser. The non-threaded runtime is no longer
-- supported.
--
-- This is an example of an algorithm where code coverage could be high,
-- even 100%, but the behavior is still under tested. I should collect
-- more of these examples...
--
-- reference: http://www.unixwiz.net/techtips/termios-vmin-vtime.html
module Graphics.Vty.Input.Loop
  ( Input(..)
  , eventChannel

  , initInput
  )
where

import Graphics.Vty.Config
import Graphics.Vty.Input.Classify
import Graphics.Vty.Input.Events

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (mask, try, SomeException)
import Lens.Micro hiding ((<>~))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Control.Monad (when, mzero, forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.Trans.Reader (ReaderT(..))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Word (Word8)

import Foreign (allocaArray, allocaBytes)
import Foreign.C.Types -- (CInt(..))
import Foreign.Ptr (Ptr, castPtr)

import System.IO
import Text.Printf (hPrintf)
import Graphics.Vty.Input.BindReader
import System.Win32.Types (DWORD, withHandleToHANDLE)
import Data.Maybe (catMaybes, mapMaybe)
import Data.ByteString.UTF8

data Input = Input
    { -- | Channel of events direct from input processing. Unlike
      -- 'nextEvent' this will not refresh the display if the next event
      -- is an 'EvResize'.
      _eventChannel  :: TChan InternalEvent
      -- | Shuts down the input processing. As part of shutting down the
      -- input, this should also restore the input state.
    , shutdownInput :: IO ()
      -- | Restore the terminal's input state to what it was prior
      -- to configuring input for Vty. This should be done as part of
      -- 'shutdownInput' but is exposed in case you need to access it
      -- directly.
    , restoreInputState :: IO ()
      -- | Changes to this value are reflected after the next event.
    , _configRef :: IORef Config
      -- | input debug log
    , _inputDebug :: Maybe Handle
    }

makeLenses ''Input

data InputBuffer = InputBuffer
    { _ptrEvent :: Ptr ()
    , _ptrNum :: Ptr DWORD
    , _size :: Int
    }

makeLenses ''InputBuffer

data InputState = InputState
    { _unprocessedBytes :: ByteString
    , _classifierState :: ClassifierState
    , _appliedConfig :: Config
    , _inputBuffer :: InputBuffer
    , _classifier :: ClassifierState -> ByteString -> KClass
    }

makeLenses ''InputState

type InputM a = StateT InputState (ReaderT Input IO) a

logMsg :: String -> InputM ()
logMsg msg = do
    d <- view inputDebug
    case d of
        Nothing -> return ()
        Just h -> liftIO $ hPutStrLn h msg >> hFlush h

-- this must be run on an OS thread dedicated to this input handling.
-- otherwise the terminal timing read behavior will block the execution
-- of the lightweight threads.
loopInputProcessor :: InputM ()
loopInputProcessor = do
    readFromDevice >>= addBytesToProcess
    validEvents <- many parseEvent
    forM_ validEvents emit
    dropInvalid
    loopInputProcessor

addBytesToProcess :: ByteString -> InputM ()
addBytesToProcess block = unprocessedBytes <>= block

emit :: Event -> InputM ()
emit event = do
    logMsg $ "parsed event: " ++ show event
    view eventChannel >>= liftIO . atomically . flip writeTChan (InputEvent event)

-- The timing requirements are assured by the VMIN and VTIME set for the
-- device.
--
-- Precondition: Under the threaded runtime. Only current use is from a
-- forkOS thread. That case satisfies precondition.
readFromDevice :: InputM ByteString
readFromDevice = do
    newConfig <- view configRef >>= liftIO . readIORef
    oldConfig <- use appliedConfig
    let Just fd = inputFd newConfig
    when (newConfig /= oldConfig) $ do
        logMsg $ "new config: " ++ show newConfig
        liftIO $ applyConfig fd newConfig
        appliedConfig .= newConfig
    bufferPtr <- use $ inputBuffer.ptrEvent
    numPtr <- use  $ inputBuffer.ptrNum
    maxBytes  <- use $ inputBuffer.size
    stringRep <- liftIO $ do
        {-
            One HANDLE allocation per read, we can do better
        -}

        evs <- withHandleToHANDLE fd $ \wh -> do
            evsH <- eventReader wh bufferPtr numPtr
            pure $ mapMaybe usefulEvent evsH
        pure $ fromString evs

    unless (BS.null stringRep) $
        logMsg $ "input bytes: " ++ show (BS8.unpack stringRep)
    return stringRep

applyConfig :: Handle -> Config -> IO ()
applyConfig fd (Config{ vmin = Just theVmin, vtime = Just theVtime })
    = pure () -- setTermTiming fd theVmin (theVtime `div` 100) -- I trust that timings in WT are correct and I do not know how to change them
applyConfig _ _ = fail "(vty) applyConfig was not provided a complete configuration"

parseEvent :: InputM Event
parseEvent = do
    c <- use classifier
    s <- use classifierState
    b <- use unprocessedBytes
    case c s b of
        Valid e remaining -> do
            logMsg $ "valid parse: " ++ show e
            logMsg $ "remaining: " ++ show remaining
            classifierState .= ClassifierStart
            unprocessedBytes .= remaining
            return e
        _ -> mzero

dropInvalid :: InputM ()
dropInvalid = do
    c <- use classifier
    s <- use classifierState
    b <- use unprocessedBytes
    case c s b of
        Chunk -> do
            classifierState .=
                case s of
                  ClassifierStart -> ClassifierInChunk b []
                  ClassifierInChunk p bs -> ClassifierInChunk p (b:bs)
            unprocessedBytes .= BS8.empty
        Invalid -> do
            logMsg "dropping input bytes"
            classifierState .= ClassifierStart
            unprocessedBytes .= BS8.empty
        _ -> return ()

runInputProcessorLoop :: ClassifyMap -> Input -> IO ()
runInputProcessorLoop classifyTable input = do
    let eventCount = 1024
    let bufferSize = eventCount * irSize
    -- we will need to write logic for the case when we receive more that 1024 events at once
    -- maybe allocate temporary buffer in the eventReader an its friends?
    allocaArray bufferSize $ \(bufferPtr :: Ptr ()) -> do
        allocaBytes dwordSize $ \(nPtr :: Ptr DWORD) -> do
            s0 <- InputState BS8.empty ClassifierStart
                    <$> readIORef (_configRef input)
                    <*> pure (InputBuffer bufferPtr nPtr bufferSize)
                    <*> pure (classify classifyTable)
            runReaderT (evalStateT loopInputProcessor s0) input

logInitialInputState :: Input -> ClassifyMap -> IO()
logInitialInputState input classifyTable = case _inputDebug input of
    Nothing -> return ()
    Just h  -> do
        Config{ vmin = Just theVmin
              , vtime = Just theVtime
              , termName = Just theTerm } <- readIORef $ _configRef input
        _ <- hPrintf h "initial (vmin,vtime): %s\n" (show (theVmin, theVtime))
        forM_ classifyTable $ \i -> case i of
            (inBytes, EvKey k mods) -> hPrintf h "map %s %s %s %s\n" (show theTerm)
                                                                     (show inBytes)
                                                                     (show k)
                                                                     (show mods)
            _ -> return ()

initInput :: Config -> ClassifyMap -> IO Input
initInput config classifyTable = do
    let Just fd = inputFd config
    -- setFdOption fd NonBlockingRead False -- todo: deal with it later Is there a windows equivalent?
    applyConfig fd config
    stopSync <- newEmptyMVar
    input <- Input <$> atomically newTChan
                   <*> pure (return ())
                   <*> pure (return ())
                   <*> newIORef config
                   <*> maybe (return Nothing)
                             (\f -> Just <$> openFile f AppendMode)
                             (debugLog config)
    logInitialInputState input classifyTable
    inputThread <- forkOSFinally (runInputProcessorLoop classifyTable input)
                                 (\_ -> putMVar stopSync ())
    let killAndWait = do
          killThread inputThread
          takeMVar stopSync
    return $ input { shutdownInput = killAndWait }


forkOSFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkOSFinally action and_then =
  mask $ \restore -> forkOS $ try (restore action) >>= and_then

(<>=) :: (MonadState s m, Monoid a) => ASetter' s a -> a -> m ()
l <>= a = modify (l <>~ a)

(<>~) :: Monoid a => ASetter s t a a -> a -> s -> t
l <>~ n = over l (`mappend` n)
