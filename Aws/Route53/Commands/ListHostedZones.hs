{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Route53.Commands.ListHostedZones where

import           Aws.Response
import           Aws.Signature
import           Aws.Route53.Info
import           Aws.Route53.Model
--import           Aws.Route53.Metadata
--import           Aws.Route53.Query
--import           Aws.Route53.Response
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Data.Maybe
import           Text.XML.Cursor            (($//), (&|))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Network.DNS.Types          as DNS

data ListHostedZones = ListHostedZones
                     { lhzMaxNumberOfItems :: Maybe Int
                     , lhzNextToken :: Maybe T.Text
                     } deriving (Show)

data ListHostedZonesResponse = ListHostedZonesResponse
                             { lhzrHostedZones :: [HostedZone]
                             , lhzrNextToken :: Maybe T.Text
                             } deriving (Show)

listHostedZones :: ListHostedZones
listHostedZones = ListHostedZones { lhzMaxNumberOfItems = Nothing, lhzNextToken = Nothing }

-- TODO sign the date header
instance SignQuery ListHostedZones where
    type Info ListHostedZones = Route53Info
    signQuery ListHostedZones{..} = undefined
                                  --Route53SignQuery $ catMaybes
                                  --[ Just ("Action", "ListDomains")
                                  --, ("MaxNumberOfDomains",) . T.encodeUtf8 . T.pack . show <$> lhzMaxNumberOfItems
                                  --, ("NextToken",) . T.encodeUtf8 <$> lhzNextToken
                                  --]

instance ResponseConsumer r ListHostedZonesResponse where
    type ResponseMetadata ListHostedZonesResponse = Route53Metadata
    responseConsumer _ =
        route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "ListHostedZonesResponse" cursor
            let names = cursor $// elContent "HostedZone" &| parseHostedZone
            let nextToken = listToMaybe $ cursor $// elContent "NextMarker"
            return $ ListHostedZonesResponse names nextToken

instance Transaction ListHostedZones ListHostedZonesResponse
