{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Aws.Route53.Model
( HostedZone (..)
, parseHostedZone
)

where

import           Aws.Xml
import           Data.Time
import           System.Locale
import           Text.XML.Cursor (($/), ($//), (&|))
import           Data.Text.Encoding (encodeUtf8)
import qualified Control.Failure   as F
import qualified Text.XML.Cursor   as Cu
import qualified Data.Text         as T
import qualified Network.DNS.Types as DNS


data HostedZone = HostedZone 
                  { hzId :: T.Text
                  , hzName :: DNS.Domain
                  , hzCallerReference :: T.Text
                  , hzComment :: T.Text
                  , hzResourceRecordSetCount :: Int
                  } deriving (Show)

parseHostedZone :: F.Failure XmlException m => Cu.Cursor -> m HostedZone
parseHostedZone cursor = do
    id_ <- force "Missing hosted zone id" $ cursor $/ elContent "Id"
    name <- force "Missing hosted zone name" $ cursor $/ elContent "Name" &| encodeUtf8
    callerReference <- force "Missing caller reference for hosted zone" $ cursor $/ elContent "CallerReference"
    comment <- force "Missing comment for hosted zone" $ cursor $// elContent "Comment"
    resourceRecordSetCount <- textReadInt =<< (force "Missing resourceRecordCount" $ cursor $/ elContent "ResourceRecordSetCount")
    return $ HostedZone id_ name callerReference comment resourceRecordSetCount
