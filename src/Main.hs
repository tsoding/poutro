{-|
Module : Main

Entry point to the CLI utility of the animation framework
-}
module Main where

import           Display
import           Frame
import           Patreon
import           System.Environment
import           Videos.Outro

mainWithArgs :: [String] -> IO ()
mainWithArgs (outputFolder:patronsFile:aliasesFile:_) = do
  display <- return defaultDisplay
  names   <- patronNamesFromFiles patronsFile aliasesFile
  frames  <- return $ outro display names

  saveVideoToFolder display outputFolder frames
mainWithArgs _ = error "Usage: ./poutro <output-folder> <patrons.csv> <aliases.json>"

main :: IO ()
main = getArgs >>= mainWithArgs
