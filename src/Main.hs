module Main where

import           Outro
import           System.Directory
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

mainWithArgs :: [String] -> IO ()
mainWithArgs (namesFileName:outputFolder:_) = do
  display <- return defaultDisplay
  names   <- loadNamesFromFile namesFileName
  frames  <- return $ outroFromNames display names

  createDirectoryIfMissing True
                           outputFolder
  createMetaFile (printf "%s/meta.js" outputFolder)
                 (length frames)
                 (displayFps display)
  mapM_ (uncurry saveFrame)
    $ zip (frameFileNames $ printf "%s/" outputFolder) frames
mainWithArgs _ = error "Usage: ./poutro <names-file> <output-folder>"

main :: IO ()
main = getArgs >>= mainWithArgs
