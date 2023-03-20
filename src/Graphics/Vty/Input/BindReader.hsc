{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.Vty.Input.BindReader where
{-
copied from/inpired by https://github.com/judah/haskeline/issues/54
-}
import Foreign
import Foreign.C
import Graphics.Win32.Misc
import System.Win32
import System.Win32.Types
import Data.Traversable
import Control.Concurrent
import System.IO

#include <windows.h>

foreign import ccall "windows.h ReadConsoleInputW" c_ReadConsoleInput
    :: HANDLE -> Ptr () -> DWORD -> Ptr DWORD -> IO Bool

foreign import ccall "windows.h WaitForSingleObject" c_WaitForSingleObject
    :: HANDLE -> DWORD -> IO DWORD

foreign import ccall "windows.h GetNumberOfConsoleInputEvents"
    c_GetNumberOfConsoleInputEvents :: HANDLE -> Ptr DWORD -> IO Bool

dwordSize, irSize :: Int
dwordSize = #size DWORD
irSize = #size INPUT_RECORD

eventReader :: HANDLE -> Ptr () -> Ptr DWORD -> IO [InputEvent]
eventReader h ptrRec ptrNum = do
    let waitTime = 500 -- milliseconds
    ret <- c_WaitForSingleObject h waitTime
    yield -- otherwise, the above foreign call causes the loop to never
          -- respond to the killThread
    if ret /= (#const WAIT_OBJECT_0)
        then eventReader h ptrRec ptrNum
        else readEvents h ptrRec ptrNum


getNumberOfEvents :: HANDLE -> Ptr DWORD -> IO Int
getNumberOfEvents h p = do
    failIfFalse_ "GetNumberOfConsoleInputEvents"
        $ c_GetNumberOfConsoleInputEvents h p
    fmap fromEnum $ peek p


readEvents :: HANDLE -> Ptr () -> Ptr DWORD -> IO [InputEvent]
readEvents h ptrRec ptrNum = do
    n <- getNumberOfEvents h ptrNum
        -- allocaBytes (n * #size INPUT_RECORD) $ \pRecord -> do
    failIfFalse_ "ReadConsoleInput"
        $ c_ReadConsoleInput h ptrRec (toEnum n) ptrNum
    numRead <- fmap fromEnum $ peek ptrNum
    forM [0..toEnum numRead-1] $ \i -> peekEvent
        $ ptrRec `plusPtr` (i * #size INPUT_RECORD)

getKeyEvent :: Ptr () -> IO InputEvent
getKeyEvent p = do
    kDown' <- (#peek KEY_EVENT_RECORD, bKeyDown) p
    repeat' <- (#peek KEY_EVENT_RECORD, wRepeatCount) p
    keyCode <- (#peek KEY_EVENT_RECORD, wVirtualKeyCode) p
    scanCode <- (#peek KEY_EVENT_RECORD, wVirtualScanCode) p
    char :: CWchar <- (#peek KEY_EVENT_RECORD, uChar) p
    state <- (#peek KEY_EVENT_RECORD, dwControlKeyState) p
    let r = KeyEvent {keyDown = kDown',
                            repeatCount = repeat',
                            virtualKeyCode = keyCode,
                            virtualScanCode = scanCode,
                            unicodeChar = toEnum (fromEnum char),
                            controlKeyState = state}
    -- hPrint stderr r
    pure r
getWindowEvent :: Ptr () -> IO InputEvent
getWindowEvent p = do
    s <- (#peek WINDOW_BUFFER_SIZE_RECORD, dwSize) p
    return $ WindowEvent s

data InputEvent = KeyEvent {keyDown :: BOOL,
                          repeatCount :: WORD,
                          virtualKeyCode :: WORD,
                          virtualScanCode :: WORD,
                          unicodeChar :: Char,
                          controlKeyState :: DWORD}
           | WindowEvent { dwSize_ :: COORD }
           | OtherEvent
                        deriving Show

pretty :: InputEvent -> String
pretty (KeyEvent kd _ _ _ c _) = "KE { kd=" <> show kd <>", c = " <> prettyC c <> "}"
pretty y = show y

prettyC :: Char -> String
prettyC c
    | fromEnum c < 32 = show [c]
    | True = [c]

peekEvent :: Ptr () -> IO InputEvent
peekEvent pRecord = do
    eventType :: WORD <- (#peek INPUT_RECORD, EventType) pRecord
    let eventPtr = (#ptr INPUT_RECORD, Event) pRecord
    case eventType of
        (#const KEY_EVENT) -> getKeyEvent eventPtr
        (#const WINDOW_BUFFER_SIZE_EVENT) -> getWindowEvent eventPtr
        _ -> return OtherEvent


-- todo: handle window resize events
-- that requires more refactoring
usefulEvent :: InputEvent -> Maybe Char
usefulEvent (KeyEvent {..})
    | keyDown && unicodeChar == '\NUL' = Nothing
    | keyDown && unicodeChar /= '\NUL' = Just unicodeChar
    | otherwise = Nothing
usefulEvent _ = Nothing -- for now, let it compile
