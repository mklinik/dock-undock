{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import qualified Options
import           Options (Mode(..))
import DockUndock
import Xrandr
import Log

main :: IO ()
main = do
  initLogger $ \log -> do
    args <- getArgs
    logg log $ "args: " <> show args
    opts <- Options.get args
    logg log $ "opts: " <> show opts
    case Options.mode opts of
      Help -> Options.printHelp
      Dock -> xrandr >>= dock opts log
      Undock -> xrandr >>= undock
      Autodock -> do
        out <- xrandr
        if isDocked out
          then undock out
          else dock opts log out
      Mouse -> do
        pointerSetup
        keyboardSetup
