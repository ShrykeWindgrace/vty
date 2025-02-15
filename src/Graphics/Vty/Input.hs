{-# LANGUAGE RecordWildCards, CPP #-}

-- | This module provides the input layer for Vty, including methods
-- for initializing an 'Input' structure and reading 'Event's from the
-- terminal.
--
-- Note that due to the evolution of terminal emulators, some keys
-- and combinations will not reliably map to the expected events by
-- any terminal program. There is no 1:1 mapping from key events to
-- bytes read from the terminal input device. In very limited cases the
-- terminal and vty's input process can be customized to resolve these
-- issues; see "Graphics.Vty.Config" for how to configure vty's input
-- processing.
--
-- = VTY's Implementation
--
-- There are two input modes:
--
--  1. 7-bit
--
--  2. 8-bit
--
-- The 7-bit input mode is the default and the expected mode in most use
-- cases. This is what Vty uses.
--
-- == 7-bit input encoding
--
-- Control key combinations are represented by masking the two high bits
-- of the 7-bit input. Historically the control key actually grounded
-- the two high bit wires: 6 and 7. This is why control key combos
-- map to single character events: the input bytes are identical. The
-- input byte is the bit encoding of the character with bits 6 and 7
-- masked. Bit 6 is set by shift. Bit 6 and 7 are masked by control. For
-- example,
--
-- * Control-I is 'i', `01101001`, and has bit 6 and 7 masked to become
-- `00001001`, which is the ASCII and UTF-8 encoding of the Tab key.
--
-- * Control+Shift-C is 'C', `01000011`, with bit 6 and 7 set to zero
-- which is `0000011` and is the "End of Text" code.
--
-- * Hypothesis: This is why capital-A, 'A', has value 65 in ASCII: this
-- is the value 1 with bit 7 set and 6 unset.
--
-- * Hypothesis: Bit 6 is unset by upper case letters because,
-- initially, there were only upper case letters used and a 5 bit
-- encoding.
--
-- == 8-bit encoding
--
-- The 8th bit was originally used for parity checking which is useless
-- for terminal emulators. Some terminal emulators support an 8-bit
-- input encoding. While this provides some advantages, the actual usage
-- is low. Most systems use 7-bit mode but recognize 8-bit control
-- characters when escaped. This is what Vty does.
--
-- == Escaped Control Keys
--
-- Using 7-bit input encoding, the @ESC@ byte can signal the start of
-- an encoded control key. To differentiate a single @ESC@ event from a
-- control key, the timing of the input is used.
--
-- 1. @ESC@ individually: @ESC@ byte; no bytes following for a period of
-- 'VMIN' milliseconds.
--
-- 2. Control keys that contain @ESC@ in their encoding: The @ESC byte
-- is followed by more bytes read within 'VMIN' milliseconds. All bytes
-- up until the next valid input block are passed to the classifier.
--
-- If the current runtime is the threaded runtime then the terminal's
-- @VMIN@ and @VTIME@ behavior reliably implement the above rules. If
-- the current runtime does not support 'forkOS' then there is currently
-- no implementation.
--
-- == Unicode Input and Escaped Control Key Sequences
--
-- The input encoding determines how UTF-8 encoded characters are
-- recognized.
--
-- * 7-bit mode: UTF-8 can be input unambiguously. UTF-8 input is
-- a superset of ASCII. UTF-8 does not overlap escaped control key
-- sequences. However, the escape key must be differentiated from
-- escaped control key sequences by the timing of the input bytes.
--
-- * 8-bit mode: UTF-8 cannot be input unambiguously. This does not
-- require using the timing of input bytes to differentiate the escape
-- key. Many terminals do not support 8-bit mode.
--
-- == Terminfo
--
-- The terminfo system is used to determine how some keys are encoded.
-- Terminfo is incomplete and in some cases terminfo is incorrect. Vty
-- assumes terminfo is correct but provides a mechanism to override
-- terminfo; see "Graphics.Vty.Config", specifically 'inputOverrides'.
--
-- == Terminal Input is Broken
--
-- Clearly terminal input has fundamental issues. There is no easy way
-- to reliably resolve these issues.
--
-- One resolution would be to ditch standard terminal interfaces
-- entirely and just go directly to scancodes. This would be a
-- reasonable option for Vty if everybody used the linux kernel console
-- but for obvious reasons this is not possible.
--
-- The "Graphics.Vty.Config" module supports customizing the
-- input-byte-to-event mapping and xterm supports customizing the
-- scancode-to-input-byte mapping. With a lot of work a user's system
-- can be set up to encode all the key combos in an almost-sane manner.
--
-- == See also
--
-- * http://www.leonerd.org.uk/hacks/fixterms/
module Graphics.Vty.Input
  ( Key(..)
  , Modifier(..)
  , Button(..)
  , Event(..)
  , Input(..)
  , inputForConfig
  , attributeControl
  )
where

import Graphics.Vty.Config
import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Loop
import Graphics.Vty.Input.Terminfo

import Control.Concurrent.STM
import Lens.Micro

import System.IO (Handle)
import System.Win32.Console
import System.Win32.Types
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- | Set up the terminal with file descriptor `inputFd` for input.
-- Returns an 'Input'.
--
-- The table used to determine the 'Events' to produce for the input
-- bytes comes from 'classifyMapForTerm' which is then overridden by
-- the the applicable entries from the configuration's 'inputMap'.
--
-- The terminal device's mode flags are configured by the
-- 'attributeControl' function.
inputForConfig :: Config -> IO Input
inputForConfig config@Config{ termName = Just termName
                            , inputFd = Just termFd
                            , outputFd = Just outFd
                            , vmin = Just _
                            , vtime = Just _
                            , .. } = do
    let inputOverrides = [(s,e) | (t,s,e) <- inputMap, t == Nothing || t == Just termName]
        activeInputMap = classifyMapForTerm termName {- terminal -} `mappend` inputOverrides
    (setAttrs, unsetAttrs) <- attributeControl termFd outFd
    setAttrs
    input <- initInput config activeInputMap
    {-
    -- Skip this for now
    let pokeIO = Catch $ do
            setAttrs
            atomically $ writeTChan (input^.eventChannel) ResumeAfterSignal
    _ <- installHandler windowChange pokeIO Nothing
    _ <- installHandler continueProcess pokeIO Nothing
    -}

    let restore = unsetAttrs

    return $ input
        { shutdownInput = do
            shutdownInput input
            {-
            -- Skip this for now
            _ <- installHandler windowChange Ignore Nothing
            _ <- installHandler continueProcess Ignore Nothing
            -}
            restore
        , restoreInputState = restoreInputState input >> restore
        }
inputForConfig config = (<> config) <$> standardIOConfig >>= inputForConfig

-- | Construct two IO actions: one to configure the terminal for Vty and
-- one to restore the terminal mode flags to the values they had at the
-- time this function was called.
--
-- This function constructs a configuration action to clear the
-- following terminal mode flags:
--
-- * IXON disabled: disables software flow control on outgoing data.
-- This stops the process from being suspended if the output terminal
-- cannot keep up.
--
-- * Raw mode is used for input.
--
-- * ISIG (enables keyboard combinations that result in
-- signals)
--
-- * ECHO (input is not echoed to the output)
--
-- * ICANON (canonical mode (line mode) input is not used)
--
-- * IEXTEN (extended functions are disabled)
--
-- The configuration action also explicitly sets these flags:
--
-- * ICRNL (input carriage returns are mapped to newlines)

-- Conhost:
-- these flags are the bare minimum that make VTY work at all
-- we might want to experiment with ENABLE_WINDOW_INPUT
-- I have my doubts about ENABLE_PROCESSED_INPUT, I am inclined to leave it disabled
-- to my understanding, this function is only called on stdin, hence we might need to tweak stdout's flags
--   in a separate function
attributeControl :: Handle -> Handle -> IO (IO (), IO ())
attributeControl infd outfd =
    withHandleToHANDLE infd $ \wh -> do
        mode <- getConsoleMode wh
        let nOT_RAW_MODE_MASK = eNABLE_LINE_INPUT .|. eNABLE_ECHO_INPUT .|. eNABLE_PROCESSED_INPUT
        
        pure (
            -- setConsoleMode wh $ mode .&. complement nOT_RAW_MODE_MASK,
            do
                setConsoleMode wh $ (eNABLE_MOUSE_INPUT .|. eNABLE_EXTENDED_FLAGS .|. eNABLE_VIRTUAL_TERMINAL_INPUT .|. eNABLE_WINDOW_INPUT) .&. complement nOT_RAW_MODE_MASK -- .|. eNABLE_PROCESSED_INPUT,
                BS.hPut outfd $ BS8.pack "\ESC[?1004h\ESC[?1003l\ESC[?1002h" -- +focus, -mouse_any, +mouse_btn
            , do
                setConsoleMode wh mode
                BS.hPut outfd $ BS8.pack "\ESC[?1004l\ESC[?1003h\ESC[?1002l" -- -focus, +mouse_any, -mouse_btn
            )
    {- original <- getTerminalAttributes fd
    let vtyMode = foldl withMode clearedFlags flagsToSet
        clearedFlags = foldl withoutMode original flagsToUnset
        flagsToSet = [ MapCRtoLF -- ICRNL
                     ]
        flagsToUnset = [ StartStopOutput -- IXON
                       , KeyboardInterrupts -- ISIG
                       , EnableEcho -- ECHO
                       , ProcessInput -- ICANON
                       , ExtendedFunctions -- IEXTEN
                       ]
    let setAttrs = setTerminalAttributes fd vtyMode Immediately
        unsetAttrs = setTerminalAttributes fd original Immediately
    return (setAttrs, unsetAttrs)-}
