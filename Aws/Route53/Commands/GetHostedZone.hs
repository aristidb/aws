{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Aws.Route53.Commands.GetHostedZone where

import           Aws.Response
import           Aws.Signature
import           Aws.Route53.Info
import           Aws.Route53.Model
import           Aws.Route53.Metadata
import           Aws.Route53.Query
import           Aws.Route53.Response
import           Aws.Transaction
import           Aws.Xml
import qualified Network.DNS.Types          as DNS
import           Text.XML.Cursor            (($//), (&/), (&|), laxElement)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString            as B

data GetHostedZone = GetHostedZone
                   { hostedZoneId :: T.Text
                   } deriving (Show)

data GetHostedZoneResponse = GetHostedZoneResponse
                             { ghzrHostedZone :: HostedZone
                             , ghzrDelegationSet :: [DNS.Domain]
                             } deriving (Show)

getHostedZone :: T.Text -> GetHostedZone
getHostedZone hostedZoneId = GetHostedZone hostedZoneId

instance SignQuery GetHostedZone where
    type Info GetHostedZone = Route53Info
    signQuery GetHostedZone{..} = route53SignQuery path query
      where
      path = "/hostedzone/" `B.append` (T.encodeUtf8 hostedZoneId)
      query = []

instance ResponseConsumer r GetHostedZoneResponse where
    type ResponseMetadata GetHostedZoneResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "GetHostedZoneResponse" cursor
            zone <- forceM "Missing a HostedZone element" $ cursor $// laxElement "HostedZone" &| parseHostedZone
            -- TODO assert that there are exactly four nameservers
            let delegationSet = cursor $// laxElement "DelegationSet" &/ laxElement "Nameservers" &/ elContent "Nameserver" &| T.encodeUtf8
            return $ GetHostedZoneResponse zone delegationSet

instance Transaction GetHostedZone GetHostedZoneResponse where

