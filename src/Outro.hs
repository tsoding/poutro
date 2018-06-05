{-|
Module : Outro

Patreon credits outro sequence
-}
{-# LANGUAGE OverloadedStrings #-}
module Outro where

import           Animations
import           Data.String
import           Display
import           Elements
import           Frame
import           Text.Blaze.Svg (toSvg)
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import           V2

timehopEvents :: [Double]
timehopEvents = map (* (1.0 / 30.0)) [1, 22, 30, 60, 72, 83, 90, 121, 143, 150, 180, 192, 203, 210, 241]

nameSlots :: [(V2, V2)]
nameSlots = map (\(x, y) -> ( V2 x (y * 75 + 270)
                            , V2 780 (y * 75 + 270)
                            ))
              $ zip (cycle [-400, 2000]) [0 .. 8]

scene :: Display -> S.Svg -> Frame
scene display inner =
    S.docTypeSvg
     ! A.version "1.1"
     ! A.width (fromString $ show w)
     ! A.height (fromString $ show h) $
       toSvg [ S.rect
                 ! A.width (fromString $ show w)
                 ! A.height (fromString $ show h)
                 ! A.style "fill:#181818"
             , inner
             ]
    where w = displayWidth display
          h = displayHeight display

outroFromNames :: Display -> [String] -> [Frame]
outroFromNames display names =
    map (scene display)
      $ parallelCombine
      $ map (\(t, (el, (start, end))) ->
          concat [ waitFor (el start) fps t
                 , bouncyAppear (start, end) fps el
                 , waitFor (el end) fps (duration - t)
                 ])
      $ zip timehopEvents elements
    where fps = fromIntegral $ displayFps display
          elements = [(supportedBy, (V2 660 0, V2 660 150))]
                       ++ zip (map (`textElement` 60) names) nameSlots
                       ++ [ (textElement "via" 100, (V2 (-450) 1000, V2 750 1000))
                          , (patreonLogo, (V2 1920 925, V2 950 925))]
          duration = 8.5
          supportedBy = textElement "Supported by" 100
