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

timehopEvents :: [Double]
timehopEvents = map (* (1.0 / 30.0)) [1, 22, 30, 60, 72, 83, 90, 121, 143, 150, 180, 192, 203, 210, 241]

nameSlots = map (\y -> (V2 (-400) y, V2 780 y))
              $ map (+ 270)
              $ map (* 75) [0 .. 8]

defaultDisplay :: Display
defaultDisplay = Display { displayWidth = 1920
                         , displayHeight = 1080
                         , displayFps = 30
                         }

saveFrame :: FilePath -> Frame -> IO ()
saveFrame fileName svgMarkup =
    writeFile fileName $ renderSvg svgMarkup

patreonLogo :: V2 -> S.Svg
patreonLogo (V2 x y) =
    S.path
      ! (A.transform $ fromString $ printf "translate(%f, %f)" x y)
      ! A.fill "#FFFFFF"
      ! A.fillRule "evenodd"
      ! A.d "M284.367244,104 L284.367244,1.42108547e-14 L291.998128,1.42108547e-14 L291.998128,104 L284.367244,104 Z M241.879569,34.6685484 L249.462761,34.6685484 L249.462761,67.6406851 L241.502892,67.6406851 L229.631221,46.443665 L229.631221,67.6406851 L222.000337,67.6406851 L222.000337,34.6685484 L229.960206,34.6685484 L241.879569,56.1472494 L241.879569,34.6685484 Z M191.902143,33.8203912 C202.828713,33.8203912 209.329565,42.1108994 209.329565,51.1546654 C209.329565,60.1983341 202.828713,68.4889396 191.902143,68.4889396 C180.925934,68.4889396 174.425082,60.1983341 174.425082,51.1546654 C174.425082,42.1108994 180.925934,33.8203912 191.902143,33.8203912 Z M201.558521,51.1546654 C201.558521,45.7379055 197.883238,40.6977258 191.902143,40.6977258 C185.872382,40.6977258 182.245765,45.7379055 182.245765,51.1546654 C182.245765,56.5714254 185.872382,61.6115077 191.902143,61.6115077 C197.883238,61.6115077 201.558521,56.5714254 201.558521,51.1546654 Z M68.6789331,41.2626448 L68.6789331,34.6685484 L91.2399738,34.6685484 L91.2399738,41.2626448 L83.7501245,41.2626448 L83.7501245,67.6406851 L76.1195321,67.6406851 L76.1195321,41.2626448 L68.6789331,41.2626448 Z M149.555601,48.2809172 L162.037548,48.2809172 L162.037548,54.2642508 L149.555601,54.2642508 L149.555601,61.516511 L162.037548,61.516511 L162.037548,67.6406851 L141.924717,67.6406851 L141.924717,34.6685484 L162.037548,34.6685484 L162.037548,40.7927225 L149.555601,40.7927225 L149.555601,48.2809172 Z M116.95985,34.6685484 C124.259803,34.6685484 128.970707,40.1803051 128.970707,46.5860627 C128.970707,51.2021638 126.520842,55.2990922 122.422162,57.2313411 L129.0184,67.6406851 L120.16307,67.6406851 L114.320187,58.5019223 L110.74029,58.5019223 L110.74029,67.6406851 L103.110379,67.6406851 L103.110379,34.6685484 L116.95985,34.6685484 Z M121.245409,46.5860627 C121.245409,43.5239757 119.172223,40.8860646 115.969003,40.8860646 L110.74029,40.8860646 L110.74029,52.2845035 L115.969003,52.2845035 C119.172223,52.2845035 121.245409,49.6481497 121.245409,46.5860627 Z M53.7468301,63.6370987 L42.0175573,63.6370987 L40.7452241,67.6406851 L32.5954591,67.6406851 L44.3722302,34.6685484 L51.3904052,34.6685484 L63.3079195,67.6406851 L55.0174113,67.6406851 L53.7468301,63.6370987 Z M47.9042396,44.088992 L43.8531549,57.6554197 L51.8603276,57.6554197 L47.9042396,44.088992 Z M13.8006102,34.6685484 C21.1021207,34.6685484 25.8131212,40.1803051 25.8131212,46.5860627 C25.8131212,52.9918203 21.1021207,58.5019223 13.8006102,58.5019223 L7.63059242,58.5019223 L7.63059242,67.6406851 L0,67.6406851 L0,34.6685484 L13.8006102,34.6685484 Z M18.1350304,46.5860627 C18.1350304,43.5239757 16.061941,40.8860646 12.8590136,40.8860646 L7.63059242,40.8860646 L7.63059242,52.2845035 L12.8590136,52.2845035 C16.061941,52.2845035 18.1350304,49.6481497 18.1350304,46.5860627 Z"

textElement :: String -> Int -> V2 -> S.Svg
textElement label fontSize (V2 x y) =
  S.text_ (toSvg label)
    ! A.x (fromString $ show x)
    ! A.y (fromString $ show y)
    ! A.style (fromString $ printf "font-family:Cantarell;font-size:%dpx;fill:#e4e4ef" fontSize)

imageElement :: V2 -> S.Svg
imageElement (V2 x y) =
  S.image
    ! A.x (fromString $ show x)
    ! A.y (fromString $ show y)
    ! A.xlinkHref ""

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
outroFromNames display names =
    map (scene display)
      $ parallelCombine
      $ map (\(t, (el, (start, end))) ->
          concat [ waitFor (el start) fps t
                 , bouncyAppear (start, end) fps el
                 , waitFor (el end) fps (duration - t)
                 ])
      $ zip timehopEvents elements
    where fps = fromIntegral $ displayFps display
          elements = [(supportedBy, (V2 0 150, V2 660 150))]
                       ++ zip (map (\name -> textElement name 60) names) nameSlots
                       ++ [ (textElement "via" 100, (V2 (-450) 1000, V2 750 1000))
                          , (patreonLogo, (V2 1920 925, V2 950 925))]
          duration = 8.5
