{-# LANGUAGE DeriveGeneric #-}

module Parse (
    parseRecords,
) where

import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

renameFields "date" = "dateRep"
renameFields "continent" = "continentExp" 
renameFields "country" = "countriesAndTerritories" 
renameFields "population" = "popData2019"
renameFields other = other

customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Records

parseRecords :: L8.ByteString -> Either String Records
parseRecords json = eitherDecode json :: Either String Records