module DockUndock where

import System.Process
import Control.Applicative
import Data.List
import System.Process
import Control.Applicative
import Control.Monad
import Control.Exception

import Xrandr
import Config
import Options

undock :: XrandrOutput -> IO ()
undock out = do
  screenTeardown out

screenTeardown :: XrandrOutput -> IO ()
screenTeardown out = do
  callXrandr $ concatMap displayOff (otherDisplays out) <> displayAuto builtinDisplay

dock :: Options -> XrandrOutput -> IO ()
dock opts out = do
  keyboardSetup
  pointerSetup
  bellSetup
  screenSetup opts out

keyboardSetup = do
  -- keyboard repeat rate
  -- callProcess "xset" ["r", "rate", "200", "50"]
  -- keyboard map
  -- callProcess "setxkbmap" ["dvorak"]
  -- callCommand is needed for shell expansion of ~
  -- callCommand "xmodmap ~/.xmodmaprc"
  return ()

pointerSetup = do
  -- otherwise you get the ugly X
  callProcess "xsetroot" ["-cursor_name", "left_ptr"]

-- disable all beeps
bellSetup = do
  callProcess "xset" ["b", "off"]
  callProcess "xset" ["-b"]

screenSetup :: Options -> XrandrOutput -> IO ()
screenSetup opts out = case (otherDisplays out) of
  -- My Thunderbolt dock
  [otherD]
    | otherD `elem` dockDisplays -> callXrandr $
         displayMaybeMode (mainDisplayResolution opts) otherD
      <> ["--output", builtinDisplay, "--off"]
  -- single otherD is the most common case
    | otherwise -> callXrandr $
         displayAuto builtinDisplay
      <> displayMaybeMode (mainDisplayResolution opts) otherD
      <> otherD `displayLeftOf` builtinDisplay
      <> displayPrimary otherD
  -- no otherDisplays: just switch on the builtin one
  [] -> callXrandr $ displayAuto builtinDisplay
  -- multiple otherDisplays
  -- It's not clear which one should be primary, and how they should be
  -- positioned. Probably needs manual config.
  _ -> callXrandr $ displayAuto builtinDisplay <> concatMap displayAuto (otherDisplays out)
