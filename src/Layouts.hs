{-|
Module : Layouts

Collection of functions that help to properly place elements on the
Display.
-}

module Layouts ( horizontalLineLayout
               ) where

import           Display
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
