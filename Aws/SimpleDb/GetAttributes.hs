{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances #-}

module Aws.SimpleDb.GetAttributes
where

import Aws.Query
import Aws.SimpleDb.Error
import Aws.SimpleDb.Info
import Aws.SimpleDb.Model
import Aws.SimpleDb.Response
import Aws.Transaction
import Control.Monad.Compose.Class
import Text.XML.Monad

data GetAttributes
    = GetAttributes {
        gaItemName :: String
      , gaAttributeName :: Maybe String
      , gaConsistentRead :: Bool
      , gaDomainName :: String
      }
    deriving (Show)

data GetAttributesResponse
    = GetAttributesResponse {
        garAttributes :: [Attribute String]
      }
    deriving (Show)
             
getAttributes :: String -> String -> GetAttributes
getAttributes item domain = GetAttributes { gaItemName = item, gaAttributeName = Nothing, gaConsistentRead = False, gaDomainName = domain }

instance AsQuery GetAttributes SdbInfo where
    asQuery i GetAttributes{..}
        = addQuery [("Action", "GetAttributes"), ("ItemName", gaItemName), ("DomainName", gaDomainName)]
          . addQueryMaybe id ("AttributeName", gaAttributeName)
          . addQueryIf gaConsistentRead [("ConsistentRead", awsTrue)]
          $ sdbiBaseQuery i

instance SdbFromResponse GetAttributesResponse where
    sdbFromResponse = do
      testElementNameUI "GetAttributesResponse"
      attributes <- inList readAttribute <<< findElementsNameUI "Attribute"
      return $ GetAttributesResponse attributes
          where
            readAttribute = do
                        name <- strContent <<< findElementNameUI "Name"
                        value <- strContent <<< findElementNameUI "Value"
                        return $ ForAttribute name value

instance Transaction GetAttributes SdbInfo (SdbResponse GetAttributesResponse) SdbError
