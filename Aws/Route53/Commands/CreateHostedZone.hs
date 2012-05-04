{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-} 

-- | POST CreateHostedZone
--
--   Create a new Route53 hosted zone.
--
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_CreateHostedZone.html>
--
module Aws.Route53.Commands.CreateHostedZone where

import           Aws.Core
import           Aws.Route53.Info
import           Aws.Route53.Model
import           Aws.Route53.Metadata
import           Aws.Route53.Query
import           Aws.Route53.Response
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Network.DNS.Types          as DNS
import qualified Text.XML                   as XML
import           Text.Hamlet.XML            (xml)

data CreateHostedZone = CreateHostedZone
                      { chzName :: DNS.Domain
                      , chzCallerReference :: T.Text
                      , chzComment :: T.Text
                      } deriving (Show)

data CreateHostedZoneResponse = CreateHostedZoneResponse
                              { chzrHostedZone :: HostedZone
                              , chzrChangeInfo :: ChangeInfo
                              , chzrDelegationSet :: DelegationSet
                              } deriving (Show)

createHostedZone :: DNS.Domain -> T.Text -> T.Text -> CreateHostedZone
createHostedZone name callerReference comment = CreateHostedZone name callerReference comment

instance SignQuery CreateHostedZone where
    type Info CreateHostedZone = Route53Info
    signQuery CreateHostedZone{..} = route53SignQuery method resource query body
      where
      method = Post
      resource = "/hostedzone"
      query = []
      body = Just $ XML.Element "{https://route53.amazonaws.com/doc/2012-02-29/}CreateHostedZoneRequest" []
             [xml|
             <Name>#{T.decodeUtf8 chzName}
             <CallerReference>#{chzCallerReference}
             <HostedZoneConfig>
               <Comment>#{chzComment}
             |]

instance ResponseConsumer r CreateHostedZoneResponse where
    type ResponseMetadata CreateHostedZoneResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "GetHostedZoneResponse" cursor
            zone <- r53Parse cursor
            changeInfo <- r53Parse cursor
            delegationSet <- r53Parse cursor
            return $ CreateHostedZoneResponse zone changeInfo delegationSet

instance Transaction CreateHostedZone CreateHostedZoneResponse where

