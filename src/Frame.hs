{-|
Module : Frame

Frame record definiton and functions that work on the frames.
-}
module Frame where

import           Display
import           System.Directory
import           Text.Blaze.Svg.Renderer.String
import qualified Text.Blaze.Svg11 as S
import           Text.Printf

type Frame = S.Svg

frameFileNames :: String -> [String]
frameFileNames prefix = map (printf "%s%d.svg" prefix) [0 :: Int .. ]

createMetaFile :: FilePath -> Int -> Int -> IO ()
createMetaFile fileName frameCount fps =
    writeFile fileName
      $ printf "var meta = { frameCount: %d, fps: %d };" frameCount fps

saveFrame :: FilePath -> Frame -> IO ()
saveFrame fileName svgMarkup =
    writeFile fileName $ renderSvg svgMarkup

saveVideoToFolder :: Display -> FilePath -> [Frame] -> IO ()
saveVideoToFolder display outputFolder frames =
    do createDirectoryIfMissing True
                                outputFolder
       createMetaFile (printf "%s/meta.js" outputFolder)
                      (length frames)
                      (displayFps display)
       mapM_ (uncurry saveFrame)
           $ zip (frameFileNames $ printf "%s/" outputFolder) frames
