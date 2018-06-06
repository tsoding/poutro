{-|
Module : BetweenTwoSets

Animations for HaskellRank episode about solving
https://www.hackerrank.com/challenges/between-two-sets/problem
-}

module Videos.BetweenTwoSets where

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

arrayElement :: Color -> V2 Double -> Double -> S.Svg
arrayElement color (V2 cx cy) r =
    S.circle
      ! A.cx (fromString $ show cx)
      ! A.cy (fromString $ show cy)
      ! A.r (fromString $ show r)
      ! A.style (fromString $ printf "fill:%s" color)

arrayElementAppear :: Display                -- display
                   -> Double                 -- appearTime
                   -> Double                 -- freezeTime
                   -> Color                  -- color
                   -> Double                 -- radius
                   -> (V2 Double, V2 Double) -- center
                   -> [S.Svg]
arrayElementAppear display appearTime freezeTime color r (center1, center2) =
    map (\(p, r') -> arrayElement color p r')
      $ concat [ zip (bouncyAppear (center1, center2) fps appearTime)
                     (bouncyAppear (r * 2.0, r) fps appearTime)
               , zip (waitFor center2 fps freezeTime)
                     (waitFor r fps freezeTime)
               ]
    where fps = fromIntegral $ displayFps display

allArrayElementsAppear :: Display          -- display
                       -> Int              -- n
                       -> Double           -- radius
                       -> Double           -- appearTime
                       -> Double           -- freezeTime
                       -> Color            -- color
                       -> (Double, Double) -- cy1, cy2
                       -> [S.Svg]
allArrayElementsAppear display n r appearTime freezeTime color (cy1, cy2) =
    parallelCombine
      $ map (\i -> arrayElementAppear display
                                      appearTime
                                      freezeTime
                                      color
                                      r
                                      ( center1 + V2 ((2 * r + spacing) * fromIntegral i) 0
                                      , center2 + V2 ((2 * r + spacing) * fromIntegral i) 0
                                      ))
      $ [0 :: Int .. n - 1]
    where center1 = V2 cx cy1 - V2 offsetX 0
          center2 = V2 cx cy2 - V2 offsetX 0
          cx = fromIntegral width * 0.5
          offsetX = arrayImageWidth * 0.5 - r
          width = displayWidth display
          arrayImageWidth = fromIntegral n * d + fromIntegral (n - 1) * spacing
          d = 2.0 * r
          spacing = 50.0

arraysAppear :: Display         -- display
             -> Double          -- appearTime
             -> Double          -- freezeTime
             -> [Frame]
arraysAppear display appearTime freezeTime =
    map (solidBackground display backgroundColor)
      $ parallelCombine
      $ [ allArrayElementsAppear display n r appearTime freezeTime color1 (-d, cy1)
        , allArrayElementsAppear display n r appearTime freezeTime color2 (height + d, cy2)
        ]
    where color1 = "#8dff1e"
          color2 = "#ff8d1e"
          cy1 = spacing + r
          cy2 = height - spacing - r
          backgroundColor = "#181818"
          n = 6
          r = 100.0
          d = 2 * r
          spacing = 100.0
          height = fromIntegral $ displayHeight display

rules :: Display -> [Double] -> [Frame]
rules display _ =
    arraysAppear display 0.4 2
