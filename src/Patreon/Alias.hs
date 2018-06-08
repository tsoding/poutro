{-# LANGUAGE OverloadedStrings #-}
module Patreon.Alias where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import           Data.Traversable

data Alias = Alias { aliasAlias :: !T.Text
                   , aliasPermission :: !Bool
                   , aliasEmail :: !T.Text
                   } deriving Show

readAliasesFromJson :: FilePath -> IO [Alias]
readAliasesFromJson jsonFileName =
    do content <- BS.readFile jsonFileName
       jsonObj <- either error return
                 $ eitherDecode content
       result <- either error return
                   $ parseEither (\obj -> do aliases <- obj .: "aliases"
                                             for aliases $ \aliasObj ->
                                                 do alias <- aliasObj .: "alias"
                                                    permission <- aliasObj .: "permission"
                                                    email <- aliasObj .: "email"
                                                    return Alias { aliasAlias = alias
                                                                 , aliasPermission = permission
                                                                 , aliasEmail = email
                                                                 }) jsonObj
       return result
