{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module Aws.SimpleDb.PutAttributes
where

import           Aws.Query
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Control.Applicative
import           Text.XML.Monad
import qualified Data.ByteString.UTF8  as BU

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
                                       
instance AsQuery PutAttributes where
    type Info PutAttributes = SdbInfo
    asQuery i PutAttributes{..}
        = addQuery [("Action", "PutAttributes"), ("ItemName", BU.fromString paItemName), ("DomainName", BU.fromString paDomainName)]
          . addQueryList (attributeQuery setAttributeQuery) "Attribute" paAttributes
          . addQueryList (attributeQuery expectedAttributeQuery) "Expected" paExpected
          $ sdbiBaseQuery i

instance SdbFromResponse PutAttributesResponse where
    sdbFromResponse = PutAttributesResponse <$ testElementNameUI "PutAttributesResponse"

instance Transaction PutAttributes (SdbResponse PutAttributesResponse)
