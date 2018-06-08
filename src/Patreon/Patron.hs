{-# LANGUAGE OverloadedStrings #-}
module Patreon.Patron where

import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V

data Patron = Patron { patronEmail :: !T.Text
                     , patronStatus :: !T.Text
                     -- TODO: Using Double from patronLifetime is not reliable. Integer multiple of 100 is more accurate.
                     , patronLifetime :: !Double
                     } deriving Show

instance FromNamedRecord Patron where
    parseNamedRecord m = Patron <$>
                         m .: "Email" <*>
                         m .: "Status" <*>
                         m .: "Lifetime"

readPatronsFromCsv :: FilePath -> IO [Patron]
readPatronsFromCsv csvFileName =
    decodeByName <$> BS.readFile csvFileName
      >>= either error (return . V.toList . snd)
