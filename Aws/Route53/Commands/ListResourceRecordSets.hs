--
-- Copyright (c) 2012 Lars Kuhtz - http://lars.kuhtz.eu/
-- License: BSD3 (see https://raw.github.com/aristidb/aws/master/LICENSE)
--
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

-- | GET ListResourceRecordSets
--
--   Lists the resource record sets for a Route53 hosted zone. The hosted zone is identifed by
--   the hostedZoneId which is retrieved in the response to 'Aws.Route53.Commands.ListHostedZones' 
--   or 'Aws.Route53.Commands.CreateHostedZone'.
-- 
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_ListResourceRecordSets.html>
--
--   NOTE: the parameter 'identifier' is required for weighted and laltency resource record sets. This is
--   not enforced by the type.
--
module Aws.Route53.Commands.ListResourceRecordSets where

import           Aws.Core
import           Aws.Route53.Core
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Control.Applicative        ((<$>))
import           Text.XML.Cursor            (($//), (&|), ($/))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Char8      as B

data ListResourceRecordSets = ListResourceRecordSets
                   { lrrsHostedZoneId :: HostedZoneId
                   , lrrsName :: Maybe Domain
                   , lrrsRecordType :: Maybe RecordType
                   , lrrsIdentifier :: Maybe T.Text     -- ^ must be present for weighted or latency resource record sets. TODO introduce newtype wrapper
                   , lrrsMaxItems :: Maybe Int          -- ^ maximum effective value is 100
                   } deriving (Show)

-- | A most general 'ListResourceRecordSets' query
listResourceRecordSets :: HostedZoneId -> ListResourceRecordSets
listResourceRecordSets hostedZoneId = ListResourceRecordSets hostedZoneId Nothing Nothing Nothing Nothing

data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
                             { lrrsrResourceRecordSets :: ResourceRecordSets
                             , lrrsrIsTruncated :: Bool
                             , lrrsrMaxItems :: Maybe Int                 -- ^ The maxitems value from the request 
                             , lrrsrNextRecordName :: Maybe Domain        -- ^ TODO check constraint
                             , lrrsrNextRecordType :: Maybe RecordType    -- ^ TODO check constraint
                             , lrrsrNextRecordIdentifier :: Maybe T.Text  -- ^ TODO check constraint
                             } deriving (Show)

instance SignQuery ListResourceRecordSets where
    type Info ListResourceRecordSets = Route53Info
    signQuery ListResourceRecordSets{..} = route53SignQuery method resource query body
      where
      method = Get
      body = Nothing
      resource = (T.encodeUtf8 . qualifiedIdText) lrrsHostedZoneId `B.append` "/rrset"
      query = catMaybes [ ("name",) . T.encodeUtf8 . dText <$> lrrsName
                        , ("type",) . B.pack . typeToString <$> lrrsRecordType
                        , ("identifier",) . T.encodeUtf8 <$> lrrsIdentifier
                        , ("maxitems",) . B.pack . show <$> lrrsMaxItems
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
            let nextRecordName = listToMaybe $ cursor $// elContent "NextRecordName" &| Domain
            let nextRecordType = listToMaybe $ cursor $// elCont "NextRecordType" &| read
            let nextRecordIdentifier = listToMaybe $ cursor $// elContent "NextRecordIdentifier"
            return $ ListResourceRecordSetsResponse resourceRecordSets isTruncated maxItems nextRecordName nextRecordType nextRecordIdentifier

instance Transaction ListResourceRecordSets ListResourceRecordSetsResponse where

