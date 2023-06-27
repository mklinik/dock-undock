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
  callProcess "xset" ["r", "rate", "200", "50"]
  -- keyboard map
  callProcess "setxkbmap" ["dvorak"]
  -- callCommand is needed for shell expansion of ~
  callCommand "xmodmap ~/.xmodmaprc"

pointerSetup = do
  -- otherwise you get the ugly X
  callProcess "xsetroot" ["-cursor_name", "left_ptr"]
  -- Ideally these would be udev rules, but who wants to mess with that?
  --  No! Udev rules are also evaluated at boot, so if your mouse is plugged in at boot, you can't
  --  change libinput settings. It needs to be configured in xorg instead. I think I have a flat
  --  profile for all devices.
  -- When the mouse is not plugged in, these throw exceptions, which I want to ignore, because the
  -- commands themselves print an error message
  try (callProcess "xinput" ["--set-prop", "Logitech M705", "libinput Accel Profile Enabled", "1,", "0"])
    :: IO (Either SomeException ())
  try (callProcess "xinput" ["--set-prop", "Logitech M705", "libinput Accel Speed", "1.0"])
    :: IO (Either SomeException ())

-- disable all beeps
bellSetup = do
  callProcess "xset" ["b", "off"]
  callProcess "xset" ["-b"]

screenSetup :: Options -> XrandrOutput -> IO ()
screenSetup opts out = case otherDisplays out of
  [otherD]
    | otherD `elem` dockDisplays -> callXrandr $
         displayMaybeMode (mainDisplayResolution opts) otherD
      <> displayMaybeScale (scale opts) otherD
      <> ["--output", builtinDisplay, "--off"]
    | otherwise -> callXrandr $
         displayAuto builtinDisplay
      <> displayMaybeMode (mainDisplayResolution opts) otherD
      <> otherD `displayLeftOf` builtinDisplay
      <> displayPrimary otherD
  [] -> callXrandr $ displayAuto builtinDisplay
  -- multiple otherDisplays
  -- It's not clear which one should be primary, and how they should be
  -- positioned. Probably needs manual config.
  _ -> callXrandr $ displayAuto builtinDisplay <> concatMap displayAuto (otherDisplays out)
