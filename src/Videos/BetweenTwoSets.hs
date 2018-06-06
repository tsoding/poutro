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

circlePositions :: Display
                -> Int
                -> Double
                -> [V2 Double]
circlePositions display amount radius = map (\x -> V2 (x * radius * 2.0 * 2.0 + cx - shift) cy) [0 .. fromIntegral (amount - 1)]
    where V2 cx cy = displaySize * fromRational 0.5
          displaySize = fromIntegral <$> V2 (displayWidth display) (displayHeight display)
          shift = radius * 2.0 * 2.0 * fromIntegral amount * 0.5 - radius * 2.0

pulsatingCircle :: Double
                -> Double
                -> Color
                -> V2 Double
                -> [S.Svg]
pulsatingCircle fps radius color center =
    map (arrayElement color center)
      $ concat [ waitFor radius fps 2.0
               , bouncyAppear (radius, radius * 2.0) fps 0.2
               , waitFor (radius * 2.0) fps 2.0
               , bouncyAppear (radius * 2.0, radius) fps 0.2
               ]

testAnimation :: Display -> [Frame]
testAnimation display =
    map (solidBackground display backgroundColor)
      $ parallelCombine
      $ map (\(i, center) ->
                 map (arrayElement color center)
                   $ concat [ waitFor radius fps ((duration / fromIntegral circleCount) * fromIntegral i)
                            , bouncyAppear (radius, radius * 2.0) fps 0.2
                            , waitFor (radius * 2.0) fps (duration - ((duration / fromIntegral circleCount) * fromIntegral i))
                            , bouncyAppear (radius * 2.0, radius) fps 0.2
                            ])
      $ zip [1 .. circleCount]
      $ circlePositions display circleCount radius
    where backgroundColor = "#181818"
          color = "#ff8d1e"
          fps = fromIntegral $ displayFps display
          circleCount = 10
          radius = 25.0
          duration = 0.5

arrayElementAppear :: Display    -- display
                   -> Double     -- appearTime
                   -> Double     -- freezeTime
                   -> Color      -- color
                   -> Double     -- radius
                   -> V2 Double  -- center
                   -> [S.Svg]
arrayElementAppear display appearTime freezeTime color r center =
    map (\(p, r') -> arrayElement color p r')
      $ concat [ zip (bouncyAppear (V2 cx (-r), V2 cx (r + cy)) fps appearTime)
                     (bouncyAppear (r * 2.0, r) fps appearTime)
               , zip (waitFor (V2 cx (r + cy)) fps freezeTime)
                     (waitFor r fps freezeTime)
               ]
    where V2 cx cy = center
          fps = fromIntegral $ displayFps display

allArrayElementsAppear :: Display   -- display
                       -> Int       -- n
                       -> Double    -- appearTime
                       -> Double    -- freezeTime
                       -> Color     -- color
                       -> Double    -- cy
                       -> [S.Svg]
allArrayElementsAppear display n appearTime freezeTime color cy =
    parallelCombine
      $ map (\i -> arrayElementAppear display
                                      appearTime
                                      freezeTime
                                      color
                                      r
                                      (center + V2 ((2 * r + spacing) * fromIntegral i) 0))
      $ [0 :: Int .. n - 1]
    where r = 100.0
          center = V2 cx cy - V2 offsetX 0
          cx = fromIntegral width * 0.5
          offsetX = arrayImageWidth * 0.5 - r
          width = displayWidth display
          arrayImageWidth = fromIntegral n * d + fromIntegral (n - 1) * spacing
          d = 2.0 * r
          spacing = 50.0

arraysAppear :: Display -> Double -> Double -> [Frame]
arraysAppear display appearTime freezeTime =
    map (solidBackground display backgroundColor)
      $ parallelCombine
      $ [ allArrayElementsAppear display n appearTime freezeTime color1 cy1
        , allArrayElementsAppear display (n + 1) appearTime freezeTime color2 cy2
        ]
    where color1 = "#8dff1e"
          color2 = "#ff8d1e"
          cy1 = 100.0
          cy2 = 500.0
          backgroundColor = "#181818"
          n = 6

rules :: Display -> [Double] -> [Frame]
rules display _ =
    arraysAppear display 0.4 2
