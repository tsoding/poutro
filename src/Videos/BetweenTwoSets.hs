{-|
Module : BetweenTwoSets

Animations for HaskellRank episode about solving
https://www.hackerrank.com/challenges/between-two-sets/problem
-}

module Videos.BetweenTwoSets where

import           Data.String
import           Display
-- import           Elements
import           Frame
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import           Text.Printf
import           V2

type Color = String

arrayElement :: Color -> V2 -> Double -> S.Svg
arrayElement color (V2 cx cy) r =
    S.circle
      ! A.cx (fromString $ show cx)
      ! A.cy (fromString $ show cy)
      ! A.r (fromString $ show r)
      ! A.style (fromString $ printf "fill:%s" color)

rules :: Display -> [Frame]
rules = undefined
