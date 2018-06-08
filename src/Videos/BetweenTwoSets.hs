{-|
Module : BetweenTwoSets

Animations for HaskellRank episode about solving
https://www.hackerrank.com/challenges/between-two-sets/problem
-}

module Videos.BetweenTwoSets (rules) where

import           Animations
import           Display
import           Elements
import           Frame
import           V2

horizontalLineLayout :: Display     -- display
                     -> Int         -- n
                     -> Double      -- spacing
                     -> Double      -- y
                     -> [V2 Double]
horizontalLineLayout display n spacing y =
    map (\i -> V2 (fromIntegral i * spacing + offsetX) y) [0 .. n - 1]
    where offsetX = fromIntegral width * 0.5 - lineWidth * 0.5
          lineWidth = (fromIntegral n - 1) * spacing
          width = displayWidth display

rules :: Display -> [Double] -> [Frame]
rules display _ =
    let firstArray =
            parallelCombine
              $ map (\(c1, (c2, (c3, (c4, (c5, i))))) ->
                     map (\(center, r, color') -> circleElement color' center r) $
                         concat [ zip3 (waitFor c1 fps (dt * fromIntegral i * 0.25))
                                       (waitFor r1 fps (dt * fromIntegral i * 0.25))
                                       (repeat color1)
                                , zip3 (bouncyAppear (c1, c2) fps dt)
                                       (bouncyAppear (r1, r2) fps dt)
                                       (repeat color1)
                                , zip3 (waitFor c2 fps (2.0 - (dt * fromIntegral i * 0.25)))
                                       (waitFor r2 fps (2.0 - (dt * fromIntegral i * 0.25)))
                                       (repeat color1)
                                , zip3 (bouncyAppear (c2, c3) fps dt)
                                       (bouncyAppear (r2, r3) fps dt)
                                       (repeat color2)
                                , zip3 (waitFor c3 fps 2.0)
                                       (waitFor r3 fps 2.0)
                                       (repeat color2)
                                , zip3 (bouncyAppear (c3, c4) fps dt)
                                       (bouncyAppear (r3, r2) fps dt)
                                       (repeat color3)
                                , zip3 (waitFor c4 fps 2.0)
                                       (waitFor r3 fps 2.0)
                                       (repeat color3)
                                , zip3 (bouncyAppear (c4, c5) fps dt)
                                       (waitFor r3 fps dt)
                                       (repeat color3)
                                ])
              $ (  zip (horizontalLineLayout display n spacing2 y1)
                 . zip (horizontalLineLayout display n spacing2 y2)
                 . zip (repeat (V2 (width * 0.5) y3))
                 . zip (horizontalLineLayout display n spacing3 y4)
                 . zip (repeat (V2 (width + r3) y4))) [0 :: Int .. ]
            where y1 = -r2
                  y2 = 200.0
                  y3 = height * 0.40
                  y4 = height - r3 - 100.0
        secondArray =
            parallelCombine
              $ map (\(c1, (c2, (c3, i))) ->
                     map (\(center, r, color') -> circleElement color' center r) $
                         concat [ zip3 (waitFor c1 fps (dt * fromIntegral i * 0.25))
                                       (waitFor r1 fps (dt * fromIntegral i * 0.25))
                                       (repeat color3)
                                , zip3 (bouncyAppear (c1, c2) fps dt)
                                       (bouncyAppear (r1, r2) fps dt)
                                       (repeat color3)
                                , zip3 (waitFor c2 fps (2.0 - (dt * fromIntegral i * 0.25)))
                                       (waitFor r2 fps (2.0 - (dt * fromIntegral i * 0.25)))
                                       (repeat color3)
                                -- Waiting for merge of the first array
                                , zip3 (waitFor c2 fps dt)
                                       (waitFor r2 fps dt)
                                       (repeat color3)
                                , zip3 (waitFor c2 fps 2.0)
                                       (waitFor r2 fps 2.0)
                                       (repeat color3)
                                --
                                , zip3 (bouncyAppear (c2, c3) fps dt)
                                       (bouncyAppear (r2, r3) fps dt)
                                       (repeat color3)
                                ])
              $ (  zip (horizontalLineLayout display n spacing2 y1)
                 . zip (horizontalLineLayout display n spacing2 y2)
                 . zip (horizontalLineLayout display n spacing3 y2)) [0 :: Int .. ]
            where y1 = height + r2
                  y2 = height - r3 - 100.0
    in map (solidBackground display backgroundColor)
         $ parallelCombine [ firstArray
                           , secondArray
                           ]
    where backgroundColor = "#181818"
          spacing2 = d2 + 100.0
          spacing3 = d3 + 100.0
          r1 = 50.0
          r2 = 100.0
          r3 = 120.0
          d2 = 2.0 * r2
          d3 = 2.0 * r3
          fps = fromIntegral $ displayFps display
          width = fromIntegral $ displayWidth display
          height = fromIntegral $ displayHeight display
          n = 5
          dt = 0.3
          color1 = "#8dff1e"
          color2 = "#1eff8d"
          color3 = "#ff8d8d"
