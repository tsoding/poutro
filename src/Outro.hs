{-# LANGUAGE OverloadedStrings #-}
module Outro where

import           Data.List
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

timehopeEvents :: [Int]
timehopeEvents = [1, 22, 30, 60, 72, 83, 90, 121, 143, 150, 180, 192, 203, 210, 241]

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

waitFor :: S.Svg -> Double -> Double -> [S.Svg]
waitFor object fps duration = map (const object) [0 .. n]
    where dt = 1.0/ fps
          n = duration / dt

bouncyAppear :: (V2, V2) -> Double -> (V2 -> S.Svg) -> [S.Svg]
bouncyAppear (start, end) fps f =
    animate (start, overshoot) fps 0.1 f
      ++ animate (overshoot, end) fps 0.1 f
    where overshoot = end + (end - start) * V2 0.15 0.15

supportedByAnimation :: Double -> [Frame]
supportedByAnimation fps =
    concat [ bouncyAppear (start, end) fps supportedBy
           , waitFor (supportedBy end) fps 2.0
           ]
    where start = V2 x 0
          end   = V2 x 150
          x = 1920.0 / 2.0 - 300.0

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

parallelCombine :: [[S.Svg]] -> [S.Svg]
parallelCombine = map toSvg . transpose

outroFromNames :: Display -> [String] -> [Frame]
outroFromNames display _ =
    map (scene display)
      $ parallelCombine [ supportedByAnimation fps
                        , concat [ bouncyAppear (V2 500 1200, V2 500 500) fps $ textElement "hoi" 50
                                 , waitFor (textElement "hoi" 50 (V2 500 500)) fps 2.0
                                 ]
                        ]
    where fps = fromIntegral $ displayFps display
