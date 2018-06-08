{-|
Module : Videos.Outro

Patreon credits outro sequence
-}
{-# LANGUAGE OverloadedStrings #-}
module Videos.Outro (outro) where

import           Animations
import qualified Data.Text as T
import           Display
import           Elements
import           Frame
import           Layouts
import           V2

timehopEvents :: [Double]
timehopEvents = map (* (1.0 / 30.0)) [1, 22, 30, 60, 72, 83, 90, 121, 143, 150, 180, 192, 203, 210, 241]

nameSlots :: Display            -- display
          -> [(V2 Double, V2 Double)]
nameSlots display = concat [ zip (verticalLineLayout display rows spacing leftOffscreen)
                                 (verticalLineLayout display rows spacing leftColumn)
                           , zip (verticalLineLayout display rows spacing rightOffscreen)
                                 (verticalLineLayout display rows spacing rightColumn)
                           ]
    where rows = 6
          spacing = 100.0
          leftOffscreen = -500
          rightOffscreen = width
          leftColumn = 400.0
          rightColumn = width - 700.0
          width = fromIntegral $ displayWidth display

outro :: Display -> [T.Text] -> [Frame]
outro display names =
    map (solidBackground display backgroundColor)
      $ parallelCombine
      $ map (\(t, (el, (start, end))) ->
          map el $ concat [ waitFor start fps t
                          , bouncyAppear (start, end) fps 0.2
                          , waitFor end fps (duration - t)
                          ])
      $ zip timehopEvents elements
    where fps = fromIntegral $ displayFps display
          elements = [(supportedBy, (V2 660 0, V2 660 150))]
                       ++ zip (map (`textElement` 60) names) (nameSlots display)
                       ++ [ (textElement "via" 100, (V2 (-450) 1000, V2 750 1000))
                          , (patreonLogo, (V2 1920 925, V2 950 925))]
          duration = 8.5
          supportedBy = textElement "Supported by" 100
          backgroundColor = "#181818"
