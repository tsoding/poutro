module Main where

import           Outro
import           System.Environment

frameFileNames :: String -> [String]
frameFileNames prefix = map (\n -> prefix ++ show n ++ ".svg") [1 .. ]

loadNamesFromFile :: FilePath -> IO [String]
loadNamesFromFile fileName = lines <$> readFile fileName

mainWithArgs :: [String] -> IO ()
mainWithArgs [namesFileName] = do
  names <- loadNamesFromFile namesFileName
  sequence_
    $ map (uncurry saveFrame)
    $ zip (frameFileNames "frame-")
    $ outroFromNames defaultDisplay names
mainWithArgs _ = error "Usage: ./poutro <names-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
