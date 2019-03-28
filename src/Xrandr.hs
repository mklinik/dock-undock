module Xrandr where

import System.Process
import Control.Applicative
import Data.List
import Data.Char

newtype XrandrOutput = XrandrOutput [String]

-- The name of my laptop's built-in screen
builtinDisplay = "LVDS-1"

-- Queries xrandr
-- The output is supposed to be used with the other functions that take XrandrOutput arguments
xrandr :: IO XrandrOutput
xrandr = XrandrOutput . lines <$> readProcess "xrandr" ["-q"] ""

-- Gets all connected displays other than LVDS1
otherDisplays :: XrandrOutput -> [String]
otherDisplays (XrandrOutput out) =
 let
  connectedLines = filter (isInfixOf " connected") out
  connectedDisplays = map (head . words) connectedLines 
  otherDisplays = filter (/= builtinDisplay) connectedDisplays
 in
  otherDisplays

-- Gets all available modes of the given display
availableModes :: String -> XrandrOutput -> [String]
availableModes display (XrandrOutput out) =
  map (dropWhile isSpace) $ takeWhile (isPrefixOf "  ") $ tail $ dropWhile (not . isPrefixOf display) out

-- Gets the mode the display is currently using. Nothing if display is off
currentMode :: String -> XrandrOutput -> Maybe String
currentMode display out =
 let
  modes = availableModes display out
 in
  case filter (elem '*') modes of
    [] -> Nothing
    (m:_) -> Just m

-- We define the machine as docked if the built-in screen is off
isDocked :: XrandrOutput -> Bool
isDocked out = maybe True (const False) $ currentMode builtinDisplay out

displayAuto display = callProcess "xrandr" ["--output", display, "--auto"]
displayOff  display = callProcess "xrandr" ["--output", display, "--off"]
