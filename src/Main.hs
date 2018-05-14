module Main where

import           Outro
import           System.Environment
import           Text.Printf

frameFileNames :: String -> [String]
frameFileNames prefix = map (\n -> prefix ++ show n ++ ".svg") [0 .. ]

loadNamesFromFile :: FilePath -> IO [String]
loadNamesFromFile fileName = lines <$> readFile fileName

-- TODO(#8): poutro doesn't create the output folder if it doesn't exist
mainWithArgs :: [String] -> IO ()
mainWithArgs (namesFileName:outputFolder:_) = do
  names <- loadNamesFromFile namesFileName
  sequence_
    $ map (uncurry saveFrame)
    $ zip (frameFileNames $ printf "%s/" outputFolder)
    $ outroFromNames defaultDisplay names
mainWithArgs _ = error "Usage: ./poutro <names-file> <output-folder>"

main :: IO ()
main = getArgs >>= mainWithArgs
