{-# LANGUAGE 
    RecordWildCards
  , TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TupleSections
  , ScopedTypeVariables
  #-}

-- | GET ListResourceRecordSets
--
--   Lists the resource record sets for a Route53 hosted zone. The hosted zone is identifed by
--   the hostedZoneId which is retrieved in the response to 'Aws.Route53.Commands.ListHostedZones' 
--   or 'Aws.Route53.Commands.CreateHostedZone'.
-- 
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_ListResourceRecordSets.html>
--
--   NOTE: Route53 supports record type @SPF@ which is not supported in 'Network.DNS.Types' and can thus
--   not be queried through this bindings.
--
--   NOTE: the parameter 'identifier' is required for weighted and laltency resource record sets. This is
--   not enforced by the type.
--
module Aws.Route53.Commands.ListResourceRecordSets where

import           Aws.Response
import           Aws.Signature
import           Aws.Route53.Info
import           Aws.Route53.Model
import           Aws.Route53.Metadata
import           Aws.Route53.Query
import           Aws.Route53.Response
import           Aws.Transaction
import           Aws.Xml
import           Aws.Http                   (Method(..))
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Control.Applicative        ((<$>))
import qualified Network.DNS.Types          as DNS
import           Text.XML.Cursor            (($//), (&|), ($/))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Char8      as B

data ListResourceRecordSets = ListResourceRecordSets
                   { lrrsHostedZoneId :: T.Text
                   , name :: Maybe DNS.Domain
                   , recordType :: Maybe DNS.TYPE   -- ^ /note that SPF is currently not supported/
                   , identifier :: Maybe T.Text     -- ^ must be present for weighted or latency resource record sets
                   , maxitems :: Maybe Int          -- ^ maximum effective value is 100
                   } deriving (Show)

-- | A most general 'ListResourceRecordSets' query
listResourceRecordSets :: T.Text -> ListResourceRecordSets
listResourceRecordSets hostedZoneId = ListResourceRecordSets hostedZoneId Nothing Nothing Nothing Nothing

data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
                             { lrrsrResourceRecordSets :: ResourceRecordSets
                             , lrrsIsTruncated :: Bool
                             , lrrsMaxItems :: Maybe Int                 -- ^ The maxitems value from the request (TODO is it Maybe?)
                             , lrrsNextRecordName :: Maybe DNS.Domain    -- ^ TODO check constraint
                             , lrrsNextRecordType :: Maybe DNS.TYPE      -- ^ TODO check constraint
                             , lrrsNextRecordIdentifier :: Maybe T.Text  -- ^ TODO check constraint
                             } deriving (Show)

instance SignQuery ListResourceRecordSets where
    type Info ListResourceRecordSets = Route53Info
    signQuery ListResourceRecordSets{..} = route53SignQuery method resource query
      where
      method = Get
      resource = "/hostedzone/" `B.append` (T.encodeUtf8 lrrsHostedZoneId) `B.append` "/rrset"
      query = catMaybes [ ("name",) <$> name
                        , ("type",) . B.pack . typeToString <$> recordType
                        , ("identifier",) . T.encodeUtf8 <$> identifier
                        , ("maxitems",) . B.pack . show <$> maxitems
                        ]

instance ResponseConsumer r ListResourceRecordSetsResponse where
    type ResponseMetadata ListResourceRecordSetsResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parser
        where 
        parser cursor = do
            route53CheckResponseType () "ListResourceRecordSetsResponse" cursor
            resourceRecordSets <- r53Parse cursor
            isTruncated <- force "Missing IsTruncated element" $ cursor $/ elCont "IsTruncated" &| ("True"==)
            maxItems <- listToMaybe <$> (sequence $ cursor $/ elCont "MaxItems" &| readInt)
            let nextRecordName = listToMaybe $ cursor $// elContent "NextRecordName" &| T.encodeUtf8
            let nextRecordType = listToMaybe $ cursor $// elCont "NextRecordType" &| DNS.toType
            let nextRecordIdentifier = listToMaybe $ cursor $// elContent "NextRecordIdentifier"
            return $ ListResourceRecordSetsResponse resourceRecordSets isTruncated maxItems nextRecordName nextRecordType nextRecordIdentifier

instance Transaction ListResourceRecordSets ListResourceRecordSetsResponse where

