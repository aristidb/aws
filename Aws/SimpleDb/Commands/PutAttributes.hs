{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.PutAttributes
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
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

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
    type Info PutAttributes = SdbInfo
    signQuery PutAttributes{..}
        = sdbSignQuery $ 
            [("Action", "PutAttributes"), ("ItemName", T.encodeUtf8 paItemName), ("DomainName", T.encodeUtf8 paDomainName)] ++
            queryList (attributeQuery setAttributeQuery) "Attribute" paAttributes ++
            queryList (attributeQuery expectedAttributeQuery) "Expected" paExpected

instance ResponseIteratee r PutAttributesResponse where
    type ResponseMetadata PutAttributesResponse = SdbMetadata
    responseIteratee _ = sdbResponseIteratee $ sdbCheckResponseType PutAttributesResponse "PutAttributesResponse"

instance Transaction PutAttributes PutAttributesResponse
