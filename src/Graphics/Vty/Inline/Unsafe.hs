{-# OPTIONS_HADDOCK hide #-}

module Graphics.Vty.Inline.Unsafe
  ( withOutput
  , withVty
  )
where

import Graphics.Vty

import Data.IORef

import GHC.IO.Handle (hDuplicate)

import System.IO (stdin, stdout, hSetBuffering, BufferMode(NoBuffering))

import System.IO.Unsafe

globalVty :: IORef (Maybe Vty)
{-# NOINLINE globalVty #-}
globalVty = unsafePerformIO $ newIORef Nothing

globalOutput :: IORef (Maybe Output)
{-# NOINLINE globalOutput #-}
globalOutput = unsafePerformIO $ newIORef Nothing

mkDupeConfig :: IO Config
mkDupeConfig = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    stdinDupe <- hDuplicate stdin
    stdoutDupe <- hDuplicate stdout
    return $ defaultConfig { inputFd = Just stdinDupe, outputFd = Just stdoutDupe }

-- | This will create a Vty instance using 'mkVty' and execute an IO
-- action provided that instance. The created Vty instance will be
-- stored to the unsafe 'IORef' 'globalVty'.
--
-- This instance will use duplicates of the stdin and stdout Handles.
withVty :: (Vty -> IO b) -> IO b
withVty f = do
    mvty <- readIORef globalVty
    vty <- case mvty of
        Nothing -> do
            vty <- mkDupeConfig >>= mkVty
            writeIORef globalVty (Just vty)
            return vty
        Just vty -> return vty
    f vty

withOutput :: (Output -> IO b) -> IO b
withOutput f = do
    mout <- readIORef globalOutput
    out <- case mout of
        Nothing -> do
            config <- mappend <$> userConfig <*> mkDupeConfig
            out <- outputForConfig config
            writeIORef globalOutput (Just out)
            return out
        Just out -> return out
    f out
