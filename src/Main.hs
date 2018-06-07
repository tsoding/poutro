{-|
Module : Main

Entry point to the CLI utility of the animation framework
-}
module Main where

import           Display
import           Frame
import           System.Environment
import           Videos.Outro
-- import           Videos.BetweenTwoSets

loadNamesFromFile :: FilePath -> IO [String]
loadNamesFromFile fileName = lines <$> readFile fileName

mainWithArgs :: [String] -> IO ()
mainWithArgs (namesFileName:outputFolder:_) = do
  display <- return defaultDisplay
  names   <- loadNamesFromFile namesFileName
  frames  <- return $ outro display names

  saveVideoToFolder display outputFolder frames
mainWithArgs _ = error "Usage: ./poutro <names-file> <output-folder>"

main :: IO ()
main = getArgs >>= mainWithArgs
