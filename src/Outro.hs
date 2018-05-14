{-# LANGUAGE OverloadedStrings #-}
module Outro where

import           Data.String
import           Text.Blaze.Svg.Renderer.String
import           Text.Blaze.Svg11 ((!), mkPath, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

type Frame = S.Svg
data Display = Display { displayWidth :: Int
                       , displayHeight :: Int
                       , displayFps :: Int
                       }

defaultDisplay :: Display
defaultDisplay = Display { displayWidth = 1920
                         , displayHeight = 1080
                         , displayFps = 30
                         }

saveFrame :: FilePath -> Frame -> IO ()
saveFrame fileName svgMarkup =
    writeFile fileName $ renderSvg svgMarkup

outroFromNames :: Display -> [String] -> [Frame]
outroFromNames display _ =
    map (circleAt display) $ animateCircle display 5.0

circleAt :: Display -> (Double, Double) -> Frame
circleAt display (x, y) =
    S.docTypeSvg
      ! A.version "1.1"
      ! A.width (fromString $ show w)
      ! A.height (fromString $ show h) $
        S.circle
          ! A.cx (fromString $ show x)
          ! A.cy (fromString $ show y)
          ! A.r (fromString $ show r)
          ! A.fill "red"
    where w = displayWidth display
          h = displayHeight display
          r = fromIntegral w * 0.1

animateCircle :: Display -> Double -> [(Double, Double)]
animateCircle display s = map (\x -> (x * dx + x0, y))
                              [0.0 .. s / dt]
    where r = w * 0.1
          x0 = r
          x1 = w - r
          d = x1 - x0
          y  = h * 0.5
          w = fromIntegral $ displayWidth display
          h = fromIntegral $ displayHeight display
          fps = fromIntegral $ displayFps display
          dt = 1.0 / fps
          dx = dt / s * d
