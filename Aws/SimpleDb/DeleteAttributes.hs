{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.DeleteAttributes
where

import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import           Control.Applicative
import           Text.XML.Monad
import qualified Data.ByteString.UTF8  as BU

data DeleteAttributes
    = DeleteAttributes {
        daItemName :: String
      , daAttributes :: [Attribute DeleteAttribute]
      , daExpected :: [Attribute ExpectedAttribute]
      , daDomainName :: String
      }
    deriving (Show)

data DeleteAttributesResponse
    = DeleteAttributesResponse
    deriving (Show)
             
deleteAttributes :: String -> [Attribute DeleteAttribute] -> String -> DeleteAttributes
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
            [("Action", "DeleteAttributes"), ("ItemName", BU.fromString daItemName), ("DomainName", BU.fromString daDomainName)] ++
            queryList (attributeQuery deleteAttributeQuery) "Attribute" daAttributes ++
            queryList (attributeQuery expectedAttributeQuery) "Expected" daExpected

instance SdbFromResponse DeleteAttributesResponse where
    sdbFromResponse = DeleteAttributesResponse <$ testElementNameUI "DeleteAttributesResponse"

instance Transaction DeleteAttributes (SdbResponse DeleteAttributesResponse)
