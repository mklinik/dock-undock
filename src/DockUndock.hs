module DockUndock where

import System.Process
import Control.Applicative
import Data.List
import System.Process
import Control.Applicative
import Control.Monad

import Xrandr
import Config

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

screenSetup :: XrandrOutput -> IO ()
screenSetup out = case (otherDisplays out) of
  -- single otherD is the most common case
  [otherD] -> callXrandr $
       displayAuto builtinDisplay
    <> displayAuto otherD
    <> otherD `displayLeftOf` builtinDisplay
    <> displayPrimary otherD
  -- no otherDisplays: just switch on the builtin one
  [] -> callXrandr $ displayAuto builtinDisplay
  -- multiple otherDisplays
  -- It's not clear which one should be primary, and how they should be
  -- positioned. Probably needs manual config.
  _ -> callXrandr $ displayAuto builtinDisplay <> concatMap displayAuto (otherDisplays out)
