{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.RemovePermission where

import           Aws.Response
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

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
        parse _ = do
          return RemovePermissionResponse {}  
          
instance SignQuery RemovePermission  where 
    type Info RemovePermission  = SqsInfo
    signQuery RemovePermission {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just rpQueueName, 
                                             sqsQuery = [("Action", Just "RemovePermission"), 
                                                        ("Label", Just $ TE.encodeUtf8 rpLabel )]} 

instance Transaction RemovePermission RemovePermissionResponse



