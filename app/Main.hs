{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import qualified Options
import           Options (Mode(..))
import DockUndock
import Xrandr
import System.Log.FastLogger
import System.Log.FastLogger.Date

logg :: ToLogStr a => TimedFastLogger -> a -> IO ()
logg logger msg = logger (\time -> toLogStr time <> " " <> (toLogStr msg) <> "\n")

main :: IO ()
main = do
  t <- newTimeCache "%Y-%d-%m %T"
  withTimedFastLogger t (LogFileNoRotate "/home/mkl/dock.log" defaultBufSize) $ \f -> do
    args <- getArgs
    logg f $ show args
    opts <- Options.get args
    case Options.mode opts of
      Help -> Options.printHelp
      Dock -> xrandr >>= dock opts
      Undock -> xrandr >>= undock
      Autodock -> do
        out <- xrandr
        if isDocked out
          then undock out
          else dock opts out
      Mouse -> do
        pointerSetup
        keyboardSetup
