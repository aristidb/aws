{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | GET GetHostedZone
--
--   Get a particular Route53 hosted zone identified through its 'hostedZoneId'.
--   The HostedZoneId is obtained in the response to 'Aws.Route53.Commands.CreateHostedZone'
--   or 'Aws.Route53.Commands.ListHostedZones'
--
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_GetHostedZone.html>
--
module Aws.Route53.Commands.GetHostedZone where

import           Aws.Response
import           Aws.Signature
import           Aws.Route53.Info
import           Aws.Route53.Model
import           Aws.Route53.Metadata
import           Aws.Route53.Query
import           Aws.Route53.Response
import           Aws.Transaction
import           Aws.Http                   (Method(..))
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
    signQuery GetHostedZone{..} = route53SignQuery method resource query Nothing
      where
      method = Get
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

