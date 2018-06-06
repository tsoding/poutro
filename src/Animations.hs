{-|
Module : Animations

All common functions that produce sequence of frames based on some
input.
-}
module Animations where

import           Data.List
import           Text.Blaze.Svg (toSvg)
import qualified Text.Blaze.Svg11 as S

animate :: (Fractional a, Num a) => (a, a) -> Double -> Double -> (a -> S.Svg) -> [S.Svg]
animate (startPos, endPos) fps duration f = map (\i -> f (startPos + dv * fromInteger i)) [0 .. n]
    where dt = 1.0 / fps
          n = round (duration / dt)
          dv = (endPos - startPos) / fromInteger n

waitFor :: S.Svg -> Double -> Double -> [S.Svg]
waitFor object fps duration = map (const object) [0 .. n]
    where dt = 1.0 / fps
          n = duration / dt

bouncyAppear :: (Fractional a, Num a) => (a, a) -> Double -> (a -> S.Svg) -> [S.Svg]
bouncyAppear (start, end) fps f =
    animate (start, overshoot) fps 0.1 f
      ++ animate (overshoot, end) fps 0.1 f
    where overshoot = end + (end - start) * fromRational 0.15

parallelCombine :: [[S.Svg]] -> [S.Svg]
parallelCombine = map toSvg . transpose
