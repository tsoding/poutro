{-# LANGUAGE OverloadedStrings #-}
module Patreon where

import           Data.Function
import           Data.List
import qualified Data.Text as T
import           Patreon.Alias
import           Patreon.Patron

aliasByPatron :: Patron -> [Alias] -> Alias
aliasByPatron patron aliases =
    case filter (\alias -> aliasEmail alias == patronEmail patron) aliases of
      [alias] -> alias
      []      -> error "Couldn't find alias for patron"
      _       -> error "There are several patrons with the same email"

patronGavePermission :: Patron -> [Alias] -> Bool
patronGavePermission patron aliases =
    aliasPermission $ aliasByPatron patron aliases

patronAlias :: [Alias] -> Patron -> T.Text
patronAlias aliases patron =
    aliasAlias $ aliasByPatron patron aliases

patronNames :: [Patron] -> [Alias] -> [T.Text]
patronNames patrons aliases =
    map (patronAlias aliases) $
    sortBy (flip compare `on` patronLifetime) $
    sortBy (compare `on` patronAlias aliases) $
    filter (\patron -> patronStatus patron == "Active patron"
                       && patronGavePermission patron aliases)
           patrons

patronNamesFromFiles :: FilePath -- patronsFilePath
                     -> FilePath -- aliasesFilePath
                     -> IO [T.Text]
patronNamesFromFiles patronsFilePath aliasesFilePath =
    do patrons <- readPatronsFromCsv patronsFilePath
       aliases <- readAliasesFromJson aliasesFilePath
       return $ patronNames patrons aliases
