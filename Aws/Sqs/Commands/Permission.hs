
module Aws.Sqs.Commands.Permission where

import           Aws.Core
import           Aws.Sqs.Core
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Network.HTTP.Types    as HTTP

data AddPermission = AddPermission {
    apLabel :: T.Text,
    apPermissions :: [(T.Text,SqsPermission)],
    apQueueName :: QueueName
  } deriving (Show)

data AddPermissionResponse = AddPermissionResponse
  deriving (Show)


formatPermissions :: [(T.Text,SqsPermission)] -> [HTTP.QueryItem]
formatPermissions perms = 
  concat $ zipWith(\ x y -> [(B.pack $ "AwsAccountId." ++ show y, Just $ B.pack $ T.unpack $ fst x), 
                             (B.pack $ "ActionName." ++ show y, Just $ B.pack $ T.unpack $ printPermission $ snd x)]) perms [1 :: Integer ..]

instance ResponseConsumer r AddPermissionResponse where
    type ResponseMetadata AddPermissionResponse = SqsMetadata
    responseConsumer _ _ = sqsXmlResponseConsumer parse
       where
         parse _ = do
           return AddPermissionResponse {}
        
-- | ServiceConfiguration: 'SqsConfiguration'
instance SignQuery AddPermission  where 
    type ServiceConfiguration AddPermission  = SqsConfiguration
    signQuery AddPermission {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just apQueueName, 
                                             sqsQuery = [("Action", Just "AddPermission"), 
                                                        ("QueueName", Just $ B.pack $ T.unpack $ printQueueName apQueueName),
                                                        ("Label", Just $ B.pack $ T.unpack apLabel)] ++ formatPermissions apPermissions}

instance Transaction AddPermission AddPermissionResponse

instance AsMemoryResponse AddPermissionResponse where
    type MemoryResponse AddPermissionResponse = AddPermissionResponse
    loadToMemory = return

data RemovePermission = RemovePermission {
    rpLabel :: T.Text,
    rpQueueName :: QueueName 
  } deriving (Show)

data RemovePermissionResponse = RemovePermissionResponse 
  deriving (Show)

instance ResponseConsumer r RemovePermissionResponse where
    type ResponseMetadata RemovePermissionResponse = SqsMetadata
    responseConsumer _ _ = sqsXmlResponseConsumer parse
      where 
        parse _ = do
          return RemovePermissionResponse {}  
          
-- | ServiceConfiguration: 'SqsConfiguration'
instance SignQuery RemovePermission  where 
    type ServiceConfiguration RemovePermission  = SqsConfiguration
    signQuery RemovePermission {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just rpQueueName, 
                                             sqsQuery = [("Action", Just "RemovePermission"), 
                                                        ("Label", Just $ TE.encodeUtf8 rpLabel )]} 

instance Transaction RemovePermission RemovePermissionResponse

instance AsMemoryResponse RemovePermissionResponse where
    type MemoryResponse RemovePermissionResponse = RemovePermissionResponse
    loadToMemory = return
