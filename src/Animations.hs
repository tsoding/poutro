{-|
Module : Animations

All common functions that produce sequence of frames based on some
input.
-}
module Animations where

import           Data.List
import           Text.Blaze.Svg (toSvg)
import qualified Text.Blaze.Svg11 as S
import           V2

animate :: (V2 Double, V2 Double) -> Double -> Double -> (V2 Double -> S.Svg) -> [S.Svg]
animate (startPos, endPos) fps duration f = map (\i -> f (startPos + dv * V2 i i)) [0 .. n]
    where dt = 1.0 / fps
          n = duration / dt
          dv = (endPos - startPos) / V2 n n

waitFor :: S.Svg -> Double -> Double -> [S.Svg]
waitFor object fps duration = map (const object) [0 .. n]
    where dt = 1.0/ fps
          n = duration / dt

bouncyAppear :: (V2 Double, V2 Double) -> Double -> (V2 Double -> S.Svg) -> [S.Svg]
bouncyAppear (start, end) fps f =
    animate (start, overshoot) fps 0.1 f
      ++ animate (overshoot, end) fps 0.1 f
    where overshoot = end + (end - start) * V2 0.15 0.15

parallelCombine :: [[S.Svg]] -> [S.Svg]
parallelCombine = map toSvg . transpose
