module Xrandr where

import System.Process
import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe (isJust)

import Config

newtype XrandrOutput = XrandrOutput [String]

-- Queries xrandr
-- The output is supposed to be used with the other functions that take XrandrOutput arguments
xrandr :: IO XrandrOutput
xrandr = XrandrOutput . lines <$> readProcess "xrandr" ["-q"] ""

-- Gets all connected displays other than the builtin one
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

-- We define the machine as docked if there exists an otherDisplay that is currently switched on
isDocked :: XrandrOutput -> Bool
isDocked out = any isJust otherModes
  where
  otherModes = [currentMode disp out | disp <- otherDisplays out]

-- these should be used to build all arguments for xrandr, to be passed in one single command
displayAuto :: String -> [String]
displayAuto display = ["--output", display, "--auto"]
displayOff :: String -> [String]
displayOff  display = ["--output", display, "--off"]
displayLeftOf :: String -> String -> [String]
displayLeftOf leftD rightD = ["--output", leftD, "--left-of", rightD]
displayPrimary :: String -> [String]
displayPrimary display = ["--output", display, "--primary"]

callXrandr :: [String] -> IO ()
callXrandr  args = callProcess "xrandr" args
