{-# LANGUAGE OverloadedStrings #-}
module Outro where

import           Data.String
import qualified Data.Text as T
import           Text.Blaze.Svg
import           Text.Blaze.Svg.Renderer.String
import           Text.Blaze.Svg11 ((!), mkPath, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import           Text.Printf

type Frame = S.Svg
data V2 = V2 !Double !Double deriving Show
data Display = Display { displayWidth :: Int
                       , displayHeight :: Int
                       , displayFps :: Int
                       }

instance Num V2 where
    (V2 x1 y1) + (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
    (V2 x1 y1) * (V2 x2 y2) = V2 (x1 * x2) (y1 * y2)
    negate (V2 x1 y1) = V2 (negate x1) (negate y1)
    abs (V2 x1 y1) = V2 (abs x1) (abs y1)
    signum (V2 x1 y1) = V2 (signum x1) (signum y1)
    fromInteger x = V2 (fromIntegral x) (fromIntegral x)

instance Fractional V2 where
    fromRational x = V2 (fromRational x) (fromRational x)
    recip (V2 x y) = V2 (recip x) (recip y)

defaultDisplay :: Display
defaultDisplay = Display { displayWidth = 1920
                         , displayHeight = 1080
                         , displayFps = 30
                         }

saveFrame :: FilePath -> Frame -> IO ()
saveFrame fileName svgMarkup =
    writeFile fileName $ renderSvg svgMarkup

textElement :: String -> Int -> V2 -> S.Svg
textElement label fontSize (V2 x y) =
  S.text_ (toSvg label)
    ! A.x (fromString $ show x)
    ! A.y (fromString $ show y)
    ! A.style (fromString $ printf "font-family:Cantarell;font-size:%dpx;fill:#e4e4ef" fontSize)

supportedBy :: V2 -> S.Svg
supportedBy = textElement "Supported by" 100

animate :: (V2, V2) -> Double -> Double -> (V2 -> S.Svg) -> [S.Svg]
animate (startPos, endPos) fps duration f = map (\i -> f (startPos + dv * V2 i i)) [0 .. n]
    where dt = 1.0 / fps
          n = duration / dt
          dv = (endPos - startPos) / V2 n n

scene :: Display -> S.Svg -> Frame
scene display inner =
    S.docTypeSvg
     ! A.version "1.1"
     ! A.width (fromString $ show w)
     ! A.height (fromString $ show h) $
       toSvg [ S.rect
                 ! A.width (fromString $ show w)
                 ! A.height (fromString $ show h)
                 ! A.style "fill:#181818"
             , inner
             ]
    where w = displayWidth display
          h = displayHeight display

outroFromNames :: Display -> [String] -> [Frame]
outroFromNames display _ =
    map (scene display)
      $ animate (V2 x (-10), V2 x (y + 30)) fps 0.10 supportedBy
          ++ animate (V2 x (y + 30), V2 x y) fps 0.1 supportedBy
          ++ animate (V2 x y, V2 x y) fps 5.0 supportedBy
    where fps = fromIntegral $ displayFps display
          x = 1920 / 2.0 - 300.0
          y = 150
