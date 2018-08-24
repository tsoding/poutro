{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Patreon.Patron where

import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import           Text.Read

-- TODO(#24): Using Double from patronLifetime is not reliable. Integer multiple of 100 is more accurate.
newtype Dollars = Dollars { toDouble :: Double } deriving (Show, Ord, Eq)

instance FromField Dollars where
    parseField f = parseField f >>= \case
        '$':rest -> case readMaybe rest of
                      Just number -> pure $ Dollars number
                      _ -> mempty
        _ -> mempty

data Patron = Patron { patronEmail :: !T.Text
                     -- TODO(#25): patronStatus is not type safe
                     , patronStatus :: !T.Text
                     , patronLifetime :: !Dollars
                     } deriving Show


instance FromNamedRecord Patron where
    parseNamedRecord m = Patron <$>
                         m .: "Email" <*>
                         m .: "Patron Status" <*>
                         m .: "Lifetime $"

readPatronsFromCsv :: FilePath -> IO [Patron]
readPatronsFromCsv csvFileName =
    decodeByName <$> BS.readFile csvFileName
      >>= either error (return . V.toList . snd)
