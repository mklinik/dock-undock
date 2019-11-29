module DockUndock where

import System.Process
import Control.Applicative
import Data.List
import System.Process
import Control.Applicative
import Control.Monad

import Xrandr
import Xinput

undock :: XrandrOutput -> IO ()
undock out = do
  umountDisks
  screenTeardown out

umountDisks = spawnProcess "umount" ["/media/dock"] >>= waitForProcess >> return ()

screenTeardown out = do
  mapM_ displayOff $ otherDisplays out
  displayAuto builtinDisplay

dock :: XrandrOutput -> IO ()
dock out = do
  keyboardSetup
  pointerSetup
  bellSetup
  screenSetup out

keyboardSetup = do
  keyboardMisc

pointerSetup = do
  disableAccelerationForAllPointers

keyboardMisc = do
  -- keyboard repeat rate
  callProcess "xset" ["r", "rate", "200", "50"]
  -- keyboard map
  -- callCommand is needed for shell expansion of ~
  callCommand "xmodmap ~/.xmodmaprc"

-- disable all beeps
bellSetup = do
  callProcess "xset" ["b", "off"]
  callProcess "xset" ["-b"]

screenSetup out = do
  unless (null $ otherDisplays out) $ displayOff builtinDisplay
  mapM_ displayAuto $ otherDisplays out
