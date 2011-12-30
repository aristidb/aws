{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Aws.SimpleDb.Commands.GetAttributes
where

import           Aws.Response
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Metadata
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Text.XML.Cursor            (($//), (&|))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Text.XML.Cursor            as Cu

data GetAttributes
    = GetAttributes {
        gaItemName :: T.Text
      , gaAttributeName :: Maybe T.Text
      , gaConsistentRead :: Bool
      , gaDomainName :: T.Text
      }
    deriving (Show)

data GetAttributesResponse
    = GetAttributesResponse {
        garAttributes :: [Attribute T.Text]
      }
    deriving (Show)

getAttributes :: T.Text -> T.Text -> GetAttributes
getAttributes item domain = GetAttributes { gaItemName = item, gaAttributeName = Nothing, gaConsistentRead = False, gaDomainName = domain }

instance SignQuery GetAttributes where
    type Info GetAttributes = SdbInfo
    signQuery GetAttributes{..}
        = sdbSignQuery $
            [("Action", "GetAttributes"), ("ItemName", T.encodeUtf8 gaItemName), ("DomainName", T.encodeUtf8 gaDomainName)] ++
            maybeToList (("AttributeName",) <$> T.encodeUtf8 <$> gaAttributeName) ++
            (guard gaConsistentRead >> [("ConsistentRead", awsTrue)])

instance ResponseConsumer r GetAttributesResponse where
    type ResponseMetadata GetAttributesResponse = SdbMetadata
    responseConsumer _ = sdbResponseConsumer parse
        where parse cursor = do
                sdbCheckResponseType () "GetAttributesResponse" cursor
                attributes <- sequence $ cursor $// Cu.laxElement "Attribute" &| readAttribute
                return $ GetAttributesResponse attributes

instance Transaction GetAttributes GetAttributesResponse
