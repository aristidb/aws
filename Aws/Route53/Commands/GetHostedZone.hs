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
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString            as B

data GetHostedZone = GetHostedZone
                   { hostedZoneId :: T.Text
                   } deriving (Show)

data GetHostedZoneResponse = GetHostedZoneResponse
                             { ghzrHostedZone :: HostedZone
                             , ghzrDelegationSet :: DelegationSet
                             } deriving (Show)

getHostedZone :: T.Text -> GetHostedZone
getHostedZone hostedZoneId = GetHostedZone hostedZoneId

instance SignQuery GetHostedZone where
    type Info GetHostedZone = Route53Info
    signQuery GetHostedZone{..} = route53SignQuery resource query
      where
      resource = "/hostedzone/" `B.append` (T.encodeUtf8 hostedZoneId)
      query = []

instance ResponseConsumer r GetHostedZoneResponse where
    type ResponseMetadata GetHostedZoneResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "GetHostedZoneResponse" cursor
            zone <- r53Parse cursor
            delegationSet <- r53Parse cursor
            return $ GetHostedZoneResponse zone delegationSet

instance Transaction GetHostedZone GetHostedZoneResponse where

