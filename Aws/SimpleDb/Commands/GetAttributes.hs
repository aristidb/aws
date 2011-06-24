{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Aws.SimpleDb.Commands.GetAttributes
where

import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import           Aws.Xml
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Text.XML.Enumerator.Cursor (($//), (&|))
import qualified Data.ByteString.UTF8       as BU
import qualified Text.XML.Enumerator.Cursor as Cu

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
    sdbFromResponse cursor = do
      sdbCheckResponseType () "GetAttributesResponse" cursor
      attributes <- sequence $ cursor $// Cu.laxElement "Attribute" &| readAttribute
      return $ GetAttributesResponse attributes

instance Transaction GetAttributes (SdbResponse GetAttributesResponse)
