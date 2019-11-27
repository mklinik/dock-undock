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
  -- keyboard mouse aka mousekeys
  -- NUM 1, 2, 3, 4, 6, 7, 8, 9   Direction control
  -- NUM 5  Click
  -- –  Switch to right click mode. Press 5 now to right click.
  --
  -- /  Switch to left click mode
  -- 0 (INS)  Switch to select/drag mode. Now use direction control keys to select/drag stuff
  -- . (DEL)  End select/drag
  callProcess "xkbset" ["m"]
  callProcess "xkbset" ["ma", "60", "10", "10", "5", "2"]
  callProcess "xkbset" ["exp", "=m"]

  -- keyboard repeat rate
  callProcess "xset" ["r", "rate", "200", "50"]

-- disable all beeps
bellSetup = do
  callProcess "xset" ["b", "off"]
  callProcess "xset" ["-b"]

screenSetup out = do
  unless (null $ otherDisplays out) $ displayOff builtinDisplay
  mapM_ displayAuto $ otherDisplays out
