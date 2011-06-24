{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.PutAttributes
where

import           Aws.Signature
import           Aws.SimpleDb.Error
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import           Control.Applicative
import           Text.XML.Monad
import qualified Data.ByteString.UTF8       as BU
import qualified Text.XML.Enumerator.Cursor as Cu

data PutAttributes
    = PutAttributes {
        paItemName :: String
      , paAttributes :: [Attribute SetAttribute]
      , paExpected :: [Attribute ExpectedAttribute]
      , paDomainName :: String
      }
    deriving (Show)

data PutAttributesResponse
    = PutAttributesResponse
    deriving (Show)
             
putAttributes :: String -> [Attribute SetAttribute] -> String -> PutAttributes
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
            [("Action", "PutAttributes"), ("ItemName", BU.fromString paItemName), ("DomainName", BU.fromString paDomainName)] ++
            queryList (attributeQuery setAttributeQuery) "Attribute" paAttributes ++
            queryList (attributeQuery expectedAttributeQuery) "Expected" paExpected

instance SdbFromResponse PutAttributesResponse where
    sdbFromResponse = sdbCheckResponseType PutAttributesResponse "PutAttributesResponse"

instance Transaction PutAttributes (SdbResponse PutAttributesResponse)
