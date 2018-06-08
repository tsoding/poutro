{-|
Module : Layouts

Collection of functions that help to properly place elements on the
Display.
-}

module Layouts ( horizontalLineLayout
               , verticalLineLayout ) where

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

verticalLineLayout :: Display   -- display
                   -> Int       -- n
                   -> Double    -- spacing
                   -> Double    -- x
                   -> [V2 Double]
verticalLineLayout display n spacing x =
    map (\i -> V2 x (fromIntegral i * spacing + offsetY)) [0 .. n - 1]
    where offsetY = fromIntegral height * 0.5 - lineHeight * 0.5
          lineHeight = (fromIntegral n - 1) * spacing
          height = displayHeight display
