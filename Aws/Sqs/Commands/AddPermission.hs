{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.AddPermission where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
import           Aws.Sqs.Model
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Control.Arrow         (second)
import           Control.Monad
import           Data.Enumerator              ((=$))
import           Data.Maybe
import           Data.Time.Format
import           System.Locale
import           Text.XML.Enumerator.Cursor   (($/), ($//), (&/), (&|), ($|))
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML
import qualified Network.HTTP.Types    as HTTP
import qualified Data.ByteString.UTF8  as BU
import qualified Data.ByteString.Char8 as B
import Debug.Trace

data AddPermission = AddPermission{
  apLabel :: String,
  apPermissions :: [(String,SqsPermission)],
  apQueueName :: QueueName
}deriving (Show)

data AddPermissionResponse = AddPermissionResponse{
} deriving (Show)


apParse :: Cu.Cursor -> AddPermissionResponse
apParse el = do
  AddPermissionResponse { }

formatPermissions :: [(String,SqsPermission)] -> [HTTP.QueryItem]
formatPermissions perms = 
  concat $ zipWith(\ x y -> [(B.pack $ "AwsAccountId." ++ show y, Just $ B.pack $ fst x), 
                             (B.pack $ "ActionName." ++ show y, Just $ B.pack $ printPermission $ snd x)]) perms [1..]

instance SqsResponseIteratee AddPermissionResponse where
    sqsResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                            let cursor = Cu.fromDocument doc
                                            return $ apParse cursor                                  
          
instance SignQuery AddPermission  where 
    type Info AddPermission  = SqsInfo
    signQuery AddPermission {..} = sqsSignQuery SqsQuery { 
                                             sqsQuery = [("Action", Just "AddPermission"), 
                                                        ("QueueName", Just $ B.pack $ printQueue apQueueName),
                                                        ("Label", Just $ B.pack apLabel)] ++ formatPermissions apPermissions}

instance Transaction AddPermission (SqsResponse AddPermissionResponse)
