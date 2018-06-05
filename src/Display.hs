{-|
Module : Display

Display record definition and presets
-}
module Display where

data Display = Display { displayWidth :: Int
                       , displayHeight :: Int
                       , displayFps :: Int
                       }

defaultDisplay :: Display
defaultDisplay = Display { displayWidth = 1920
                         , displayHeight = 1080
                         , displayFps = 30
                         }
