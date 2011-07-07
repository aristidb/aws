{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.RemovePermission where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
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
import qualified Data.Text.Encoding           as TE
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML
import qualified Network.HTTP.Types    as HTTP
import qualified Data.ByteString.UTF8  as BU
import qualified Data.ByteString.Char8 as B
import Debug.Trace

data RemovePermission = RemovePermission{
  rpLabel :: T.Text,
  rpQueueName :: M.QueueName 
}deriving (Show)

data RemovePermissionResponse = RemovePermissionResponse{
} deriving (Show)

instance ResponseIteratee RemovePermissionResponse where
    type ResponseMetadata RemovePermissionResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where 
        parse el = do
          return RemovePermissionResponse {}  
          
instance SignQuery RemovePermission  where 
    type Info RemovePermission  = SqsInfo
    signQuery RemovePermission {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just rpQueueName, 
                                             sqsQuery = [("Action", Just "RemovePermission"), 
                                                        ("Label", Just $ TE.encodeUtf8 rpLabel )]} 

instance Transaction RemovePermission RemovePermissionResponse



