-- ------------------------------------------------------ --
-- Copyright © 2012 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

-- | POST CreateHostedZone
--
--   Create a new Route53 hosted zone.
--
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_CreateHostedZone.html>
--
module Aws.Route53.Commands.CreateHostedZone where

import           Aws.Core
import           Aws.Route53.Core
import           Text.Hamlet.XML            (xml)
import qualified Data.Text                  as T
import           Data.Map                   (empty)
import qualified Text.XML                   as XML

data CreateHostedZone = CreateHostedZone
                      { chzName :: Domain
                      , chzCallerReference :: T.Text
                      , chzComment :: T.Text
                      } deriving (Show)

data CreateHostedZoneResponse = CreateHostedZoneResponse
                              { chzrHostedZone :: HostedZone
                              , chzrChangeInfo :: ChangeInfo
                              , chzrDelegationSet :: DelegationSet
                              } deriving (Show)

createHostedZone :: Domain -> T.Text -> T.Text -> CreateHostedZone
createHostedZone name callerReference comment = CreateHostedZone name callerReference comment

-- | ServiceConfiguration: 'Route53Configuration'
instance SignQuery CreateHostedZone where
    type ServiceConfiguration CreateHostedZone = Route53Configuration
    signQuery CreateHostedZone{..} = route53SignQuery method resource query body
      where
      method = Post
      resource = "/hostedzone"
      query = []
      body = Just $ XML.Element "CreateHostedZoneRequest" empty
             [xml|
             <Name>#{dText chzName}
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

instance Transaction CreateHostedZone CreateHostedZoneResponse

