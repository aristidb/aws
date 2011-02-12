{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, OverloadedStrings, TupleSections #-}

module Aws.SimpleDb.GetAttributes
where

import           Aws.Query
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Compose.Class
import           Data.Maybe
import           Text.XML.Monad
import qualified Data.ByteString.UTF8        as BU

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

instance SignQuery GetAttributes where
    type Info GetAttributes = SdbInfo
    signQuery GetAttributes{..}
        = sdbSignQuery $
            [("Action", "GetAttributes"), ("ItemName", BU.fromString gaItemName), ("DomainName", BU.fromString gaDomainName)] ++
            maybeToList (("AttributeName",) <$> BU.fromString <$> gaAttributeName) ++
            (guard gaConsistentRead >> [("ConsistentRead", awsTrue)])

instance SdbFromResponse GetAttributesResponse where
    sdbFromResponse = do
      testElementNameUI "GetAttributesResponse"
      attributes <- inList readAttribute <<< findElementsNameUI "Attribute"
      return $ GetAttributesResponse attributes

instance Transaction GetAttributes (SdbResponse GetAttributesResponse)
