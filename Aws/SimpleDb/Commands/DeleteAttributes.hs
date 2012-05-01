{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.DeleteAttributes
where

import           Aws.Core
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Metadata
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

data DeleteAttributes
    = DeleteAttributes {
        daItemName :: T.Text
      , daAttributes :: [Attribute DeleteAttribute]
      , daExpected :: [Attribute ExpectedAttribute]
      , daDomainName :: T.Text
      }
    deriving (Show)

data DeleteAttributesResponse
    = DeleteAttributesResponse
    deriving (Show)
             
deleteAttributes :: T.Text -> [Attribute DeleteAttribute] -> T.Text -> DeleteAttributes
deleteAttributes item attributes domain = DeleteAttributes { 
                                         daItemName = item
                                       , daAttributes = attributes
                                       , daExpected = []
                                       , daDomainName = domain 
                                       }
                                       
instance SignQuery DeleteAttributes where
    type Info DeleteAttributes = SdbInfo
    signQuery DeleteAttributes{..}
        = sdbSignQuery $ 
            [("Action", "DeleteAttributes"), ("ItemName", T.encodeUtf8 daItemName), ("DomainName", T.encodeUtf8 daDomainName)] ++
            queryList (attributeQuery deleteAttributeQuery) "Attribute" daAttributes ++
            queryList (attributeQuery expectedAttributeQuery) "Expected" daExpected

instance ResponseConsumer r DeleteAttributesResponse where
    type ResponseMetadata DeleteAttributesResponse = SdbMetadata
    responseConsumer _ = sdbResponseConsumer $ sdbCheckResponseType DeleteAttributesResponse "DeleteAttributesResponse"

instance Transaction DeleteAttributes DeleteAttributesResponse
