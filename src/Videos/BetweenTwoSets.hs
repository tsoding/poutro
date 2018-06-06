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
               , bouncyAppear (radius, radius * 2.0) fps
               , waitFor (radius * 2.0) fps 2.0
               , bouncyAppear (radius * 2.0, radius) fps
               ]

rules :: Display -> [Frame]
rules display =
    map (solidBackground display backgroundColor)
      $ parallelCombine
      $ map (\(i, center) ->
                 map (arrayElement color center)
                   $ concat [ waitFor radius fps ((duration / fromIntegral circleCount) * fromIntegral i)
                            , bouncyAppear (radius, radius * 2.0) fps
                            , waitFor (radius * 2.0) fps (duration - ((duration / fromIntegral circleCount) * fromIntegral i))
                            , bouncyAppear (radius * 2.0, radius) fps
                            ])
      $ zip [1 .. circleCount]
      $ circlePositions display circleCount radius
    where backgroundColor = "#181818"
          color = "#ff8d1e"
          fps = fromIntegral $ displayFps display
          circleCount = 10
          radius = 25.0
          duration = 0.5
