module Options where

import Control.Monad
import System.Console.GetOpt
import System.Exit

data Mode = Dock | Undock | Autodock | Mouse | Help

data Options = Options
  { mode :: Mode
  , mainDisplayResolution :: Maybe String
  }

defaultOptions :: Options
defaultOptions  = Options
  { mode = Dock
  , mainDisplayResolution = Nothing
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] (NoArg  (\opts -> opts { mode = Help })) "display this help and exit"
  , Option [] ["dock"] (NoArg  (\opts -> opts { mode = Dock })) "dock laptop"
  , Option [] ["undock"] (NoArg  (\opts -> opts { mode = Undock })) "undock laptop"
  , Option [] ["toggle"] (NoArg  (\opts -> opts { mode = Autodock })) "automatically determine if it should be dock or undock"
  , Option [] ["mouse"] (NoArg  (\opts -> opts { mode = Mouse })) "only perform mouse pointer and keyboard setup"
  , Option [] ["resolution"] (ReqArg (\mode opts -> opts { mainDisplayResolution = Just mode } ) "RESOLUTION") "set main display to RESOLUTION"
  , Option [] ["chordify"] (NoArg (\opts -> opts { mode = Dock, mainDisplayResolution = Just "2560x1440" } )) "like --dock but set main display to 2560x1440"
  ]

get :: [String] -> IO Options
get args = do
  let (opts_, files, errors) = getOpt Permute options args
  let opts = foldl (flip id) defaultOptions opts_

  when ((not . null) errors)
    (tryHelp $ head errors)

  when ((not . null) files)
    (tryHelp $ "unrecognized option `" ++ head files ++ "'\n")

  return opts

  where
    printAndExit :: String -> IO a
    printAndExit s = putStr s >> exitFailure

    tryHelp message = printAndExit $ "dock: " ++ message
      ++ "Try `dock --help' for more information.\n"

printHelp :: IO ()
printHelp = putStr $ usageInfo "Usage: dock [OPTION]...\n" options
