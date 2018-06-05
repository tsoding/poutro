{-|
Module : Frame

Frame record definiton and functions that work on the frames.
-}
module Frame where

import           Data.List
import           Text.Blaze.Svg (toSvg)
import           Text.Blaze.Svg.Renderer.String
import qualified Text.Blaze.Svg11 as S

type Frame = S.Svg

saveFrame :: FilePath -> Frame -> IO ()
saveFrame fileName svgMarkup =
    writeFile fileName $ renderSvg svgMarkup

parallelCombine :: [[S.Svg]] -> [S.Svg]
parallelCombine = map toSvg . transpose
