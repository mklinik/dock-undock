module Config where

-- The name of my laptop's built-in screen
builtinDisplay :: String
builtinDisplay = "eDP1"

-- The name of my dock's screens; depends on whether I have connected the left or right socket
dockDisplays :: [String]
dockDisplays = ["DP1-2", "DP3-2", "DP1"] -- TODO DP1 is probably not what I want, it disables two-screen setup (which I never use nowadays anyway)
