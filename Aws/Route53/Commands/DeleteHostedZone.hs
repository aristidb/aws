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

data DeleteHostedZone = DeleteHostedZone
                   { dhzHostedZoneId :: T.Text
                   } deriving (Show)

data DeleteHostedZoneResponse = DeleteHostedZoneResponse
                              { dhzrChangeInfo :: ChangeInfo
                              } deriving (Show)

deleteHostedZone :: T.Text -> DeleteHostedZone
deleteHostedZone hostedZoneId = DeleteHostedZone hostedZoneId

instance SignQuery DeleteHostedZone where
    type Info DeleteHostedZone = Route53Info
    signQuery DeleteHostedZone{..} = route53SignQuery method resource query
      where
      method = Delete
      resource = "/hostedzone/" `B.append` (T.encodeUtf8 dhzHostedZoneId)
      query = []

instance ResponseConsumer r DeleteHostedZoneResponse where
    type ResponseMetadata DeleteHostedZoneResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "DeleteHostedZoneResponse" cursor
            changeInfo <- r53Parse cursor
            return $ DeleteHostedZoneResponse changeInfo

instance Transaction DeleteHostedZone DeleteHostedZoneResponse where

