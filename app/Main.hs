module Main where

import System.Environment (getArgs)

import qualified Options
import           Options (Mode(..))
import DockUndock
import Xrandr

main :: IO ()
main = do
  args <- getArgs
  opts <- Options.get args
  case Options.mode opts of
    Help -> Options.printHelp
    Dock -> xrandr >>= dock
    Undock -> xrandr >>= undock
    Autodock -> do
      out <- xrandr
      if isDocked out
        then undock out
        else dock out
    Mouse -> pointerSetup
