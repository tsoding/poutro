{-|
Module : BetweenTwoSets

Animations for HaskellRank episode about solving
https://www.hackerrank.com/challenges/between-two-sets/problem
-}

module Videos.BetweenTwoSets (rules) where

import           Animations
import           Data.String
import           Display
import           Elements
import           Frame
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import           Text.Printf
import           V2

type Color = String

circleElement :: Color           -- color
              -> V2 Double       -- center
              -> Double          -- radius
              -> S.Svg
circleElement color (V2 cx cy) r =
    S.circle
      ! A.cx (fromString $ show cx)
      ! A.cy (fromString $ show cy)
      ! A.r (fromString $ show r)
      ! A.style (fromString $ printf "fill:%s" color)

horizontalLineLayout :: Display     -- display
                     -> Int         -- n
                     -> Double      -- spacing
                     -> Double      -- y
                     -> [V2 Double]
horizontalLineLayout display n spacing y =
    map (\x -> V2 x y)
      $ map (\i -> fromIntegral i * spacing + offsetX) [0 .. n - 1]
    where offsetX = fromIntegral width * 0.5 - lineWidth * 0.5
          lineWidth = (fromIntegral n - 1) * spacing
          width = displayWidth display

rules :: Display -> [Double] -> [Frame]
rules display _ =
    map (solidBackground display backgroundColor)
      $ parallelCombine
      $ map (map (\(center, r, color') -> circleElement color' center r))
      $ map (\(c1, (c2, (c3, (c4, i)))) ->
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
                               (waitFor r3 fps dt)
                               (repeat color2)
                        , zip3 (waitFor c4 fps 2.0)
                               (waitFor r3 fps 2.0)
                               (repeat color2)
                        ])
      $ (  zip (horizontalLineLayout display n spacing2 y1)
         . zip (horizontalLineLayout display n spacing2 y2)
         . zip (repeat (V2 (width * 0.5) y3))
         . zip (horizontalLineLayout display n spacing3 y4)) [0 :: Int .. ]
    where backgroundColor = "#181818"
          color1 = "#8dff1e"
          color2 = "#1eff8d"
          spacing2 = d2 + 100.0
          spacing3 = d3 + 100.0
          y1 = -r2
          y2 = 200.0
          y3 = height * 0.5
          y4 = height - r3 - 100.0
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
