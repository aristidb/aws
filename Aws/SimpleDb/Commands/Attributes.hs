{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Aws.SimpleDb.Commands.Attributes where

import           Aws.Core
import           Aws.SimpleDb.Core
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
    type ServiceConfiguration GetAttributes = SdbConfiguration
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

data PutAttributes
    = PutAttributes {
        paItemName :: T.Text
      , paAttributes :: [Attribute SetAttribute]
      , paExpected :: [Attribute ExpectedAttribute]
      , paDomainName :: T.Text
      }
    deriving (Show)

data PutAttributesResponse
    = PutAttributesResponse
    deriving (Show)
             
putAttributes :: T.Text -> [Attribute SetAttribute] -> T.Text -> PutAttributes
putAttributes item attributes domain = PutAttributes { 
                                         paItemName = item
                                       , paAttributes = attributes
                                       , paExpected = []
                                       , paDomainName = domain 
                                       }
                                       
instance SignQuery PutAttributes where
    type ServiceConfiguration PutAttributes = SdbConfiguration
    signQuery PutAttributes{..}
        = sdbSignQuery $ 
            [("Action", "PutAttributes"), ("ItemName", T.encodeUtf8 paItemName), ("DomainName", T.encodeUtf8 paDomainName)] ++
            queryList (attributeQuery setAttributeQuery) "Attribute" paAttributes ++
            queryList (attributeQuery expectedAttributeQuery) "Expected" paExpected

instance ResponseConsumer r PutAttributesResponse where
    type ResponseMetadata PutAttributesResponse = SdbMetadata
    responseConsumer _ = sdbResponseConsumer $ sdbCheckResponseType PutAttributesResponse "PutAttributesResponse"

instance Transaction PutAttributes PutAttributesResponse

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
    type ServiceConfiguration DeleteAttributes = SdbConfiguration
    signQuery DeleteAttributes{..}
        = sdbSignQuery $ 
            [("Action", "DeleteAttributes"), ("ItemName", T.encodeUtf8 daItemName), ("DomainName", T.encodeUtf8 daDomainName)] ++
            queryList (attributeQuery deleteAttributeQuery) "Attribute" daAttributes ++
            queryList (attributeQuery expectedAttributeQuery) "Expected" daExpected

instance ResponseConsumer r DeleteAttributesResponse where
    type ResponseMetadata DeleteAttributesResponse = SdbMetadata
    responseConsumer _ = sdbResponseConsumer $ sdbCheckResponseType DeleteAttributesResponse "DeleteAttributesResponse"

instance Transaction DeleteAttributes DeleteAttributesResponse

data BatchPutAttributes
    = BatchPutAttributes {
        bpaItems :: [Item [Attribute SetAttribute]]
      , bpaDomainName :: T.Text
      }
    deriving (Show)

data BatchPutAttributesResponse
    = BatchPutAttributesResponse
    deriving (Show)
             
batchPutAttributes :: [Item [Attribute SetAttribute]] -> T.Text -> BatchPutAttributes
batchPutAttributes items domain = BatchPutAttributes { bpaItems = items, bpaDomainName = domain }

instance SignQuery BatchPutAttributes where
    type ServiceConfiguration BatchPutAttributes = SdbConfiguration
    signQuery BatchPutAttributes{..}
        = sdbSignQuery $ 
            [("Action", "BatchPutAttributes")
            , ("DomainName", T.encodeUtf8 bpaDomainName)] ++
            queryList (itemQuery $ queryList (attributeQuery setAttributeQuery) "Attribute") "Item" bpaItems

instance ResponseConsumer r BatchPutAttributesResponse where
    type ResponseMetadata BatchPutAttributesResponse = SdbMetadata
    responseConsumer _ = sdbResponseConsumer $ sdbCheckResponseType BatchPutAttributesResponse "BatchPutAttributesResponse"

instance Transaction BatchPutAttributes BatchPutAttributesResponse

data BatchDeleteAttributes
    = BatchDeleteAttributes {
        bdaItems :: [Item [Attribute DeleteAttribute]]
      , bdaDomainName :: T.Text
      }
    deriving (Show)

data BatchDeleteAttributesResponse
    = BatchDeleteAttributesResponse
    deriving (Show)
             
batchDeleteAttributes :: [Item [Attribute DeleteAttribute]] -> T.Text -> BatchDeleteAttributes
batchDeleteAttributes items domain = BatchDeleteAttributes { bdaItems = items, bdaDomainName = domain }

instance SignQuery BatchDeleteAttributes where
    type ServiceConfiguration BatchDeleteAttributes = SdbConfiguration
    signQuery BatchDeleteAttributes{..}
        = sdbSignQuery $ 
            [("Action", "BatchDeleteAttributes")
            , ("DomainName", T.encodeUtf8 bdaDomainName)] ++
            queryList (itemQuery $ queryList (attributeQuery deleteAttributeQuery) "Attribute") "Item" bdaItems

instance ResponseConsumer r BatchDeleteAttributesResponse where
    type ResponseMetadata BatchDeleteAttributesResponse = SdbMetadata
    responseConsumer _ = sdbResponseConsumer $ sdbCheckResponseType BatchDeleteAttributesResponse "BatchDeleteAttributesResponse"

instance Transaction BatchDeleteAttributes BatchDeleteAttributesResponse
