{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.RemovePermission where

import           Aws.Core
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

data RemovePermission = RemovePermission{
  rpLabel :: T.Text,
  rpQueueName :: M.QueueName 
}deriving (Show)

data RemovePermissionResponse = RemovePermissionResponse{
} deriving (Show)

instance ResponseConsumer r RemovePermissionResponse where
    type ResponseMetadata RemovePermissionResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
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



