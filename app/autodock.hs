module Main where

import Xrandr
import DockUndock

main = do
  out <- xrandr
  if isDocked out
    then undock out
    else dock out
