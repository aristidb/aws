{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

-- | DELETE DeleteHostedZone
--
--   Delete a particular Route53 hosted zone identified through its 'hostedZoneId'.
--   The HostedZoneId is obtained in the response to 'Aws.Route53.Commands.CreateHostedZone'
--   or 'Aws.Route53.Commands.ListHostedZones'
--
--   Note that the hosted zone can be delete only after deleting all resource records other than
--   the default SOA record and the NS records.
--
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_DeleteHostedZone.html>
--
module Aws.Route53.Commands.DeleteHostedZone where

import           Aws.Core
import           Aws.Route53.Core
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString            as B

data DeleteHostedZone = DeleteHostedZone
                   { dhzHostedZoneId :: T.Text
                   } deriving (Show)

data DeleteHostedZoneResponse = DeleteHostedZoneResponse
                              { dhzrChangeInfo :: ChangeInfo
                              } deriving (Show)

deleteHostedZone :: T.Text -> DeleteHostedZone
deleteHostedZone hostedZoneId = DeleteHostedZone hostedZoneId

-- Delete add convenience methods:
-- * Delete non-empty hosted zone
-- * extract zoneId (maybe we should always strip the start of the string? Haskell is typed!)
-- * Extract bare Model from responses (that are heavily wrapped...)

instance SignQuery DeleteHostedZone where
    type Info DeleteHostedZone = Route53Info
    signQuery DeleteHostedZone{..} = route53SignQuery method resource query body
      where
      method = Delete
      resource = "/hostedzone/" `B.append` (T.encodeUtf8 dhzHostedZoneId)
      query = []
      body = Nothing

instance ResponseConsumer r DeleteHostedZoneResponse where
    type ResponseMetadata DeleteHostedZoneResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "DeleteHostedZoneResponse" cursor
            changeInfo <- r53Parse cursor
            return $ DeleteHostedZoneResponse changeInfo

instance Transaction DeleteHostedZone DeleteHostedZoneResponse where

