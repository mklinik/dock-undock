module Xinput where

import System.Process
import Control.Applicative
import Data.List
import Data.Char

allDeviceIds :: IO [String]
allDeviceIds = lines <$> readProcess "xinput" ["list", "--id-only"] "" 

deviceProperties :: String -> IO (String, [String])
deviceProperties deviceId = do
  -- first line is device name
  props <- tail <$> lines <$> readProcess "xinput" ["list-props", deviceId] ""
  return (deviceId, props)

-- All devices that have the property "libinput Accel Profile Enabled"
accelDevices :: [String] -> IO [String]
accelDevices deviceIds = do
  properties <- mapM deviceProperties deviceIds
  return $ nub
    [ deviceId
    | (deviceId, props) <- properties
    , prop <- props
    , "libinput Accel Profile Enabled" `isInfixOf` prop
    ]

disableAccelProfile :: String -> IO ()
disableAccelProfile deviceId =
  callProcess "xinput" ["set-prop", deviceId, "libinput Accel Profile Enabled", "0,", "1"]

disableAccelerationForAllPointers :: IO ()
disableAccelerationForAllPointers = do
  pointerDevices <- allDeviceIds >>= accelDevices
  mapM_ disableAccelProfile pointerDevices

main = disableAccelerationForAllPointers
