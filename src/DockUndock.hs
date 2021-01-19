module DockUndock where

import System.Process
import Control.Applicative
import Data.List
import System.Process
import Control.Applicative
import Control.Monad

import Xrandr

undock :: XrandrOutput -> IO ()
undock out = do
  screenTeardown out

screenTeardown :: XrandrOutput -> IO ()
screenTeardown out = do
  callXrandr $ concatMap displayOff (otherDisplays out) <> displayAuto builtinDisplay

dock :: XrandrOutput -> IO ()
dock out = do
  keyboardSetup
  pointerSetup
  bellSetup
  screenSetup out

keyboardSetup = do
  -- keyboard repeat rate
  callProcess "xset" ["r", "rate", "200", "50"]
  -- keyboard map
  callProcess "setxkbmap" ["dvorak"]
  -- callCommand is needed for shell expansion of ~
  callCommand "xmodmap ~/.xmodmaprc"

pointerSetup = do
  -- otherwise you get the ugly X
  callProcess "xsetroot" ["-cursor_name", "left_ptr"]

-- disable all beeps
bellSetup = do
  callProcess "xset" ["b", "off"]
  callProcess "xset" ["-b"]

screenSetup out = do
  unless (null $ otherDisplays out) $
    callXrandr $ displayOff builtinDisplay <> concatMap displayAuto (otherDisplays out)
