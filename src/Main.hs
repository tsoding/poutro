module Main where

import           Outro
import           System.Environment
import           Text.Printf

frameFileNames :: String -> [String]
frameFileNames prefix = map (\n -> prefix ++ show n ++ ".svg") [0 .. ]

loadNamesFromFile :: FilePath -> IO [String]
loadNamesFromFile fileName = lines <$> readFile fileName

createMetaFile :: FilePath -> Int -> Int -> IO ()
createMetaFile fileName frameCount fps =
    writeFile fileName
      $ printf "var meta = { frameCount: %d, fps: %d };" frameCount fps

-- TODO(#8): poutro doesn't create the output folder if it doesn't exist
mainWithArgs :: [String] -> IO ()
mainWithArgs (namesFileName:outputFolder:_) = do
  display <- return $ defaultDisplay
  names   <- loadNamesFromFile namesFileName
  frames  <- return $ outroFromNames display names
  createMetaFile (printf "%s/meta.js" outputFolder)
                 (length frames)
                 (displayFps display)
  sequence_
    $ map (uncurry saveFrame)
    $ zip (frameFileNames $ printf "%s/" outputFolder)
    $ frames
mainWithArgs _ = error "Usage: ./poutro <names-file> <output-folder>"

main :: IO ()
main = getArgs >>= mainWithArgs
