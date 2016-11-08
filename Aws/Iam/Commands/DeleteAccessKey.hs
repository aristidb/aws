{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.DeleteAccessKey
    ( DeleteAccessKey(..)
    , DeleteAccessKeyResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable
import           Prelude

-- | Deletes the access key associated with the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccessKey.html>
data DeleteAccessKey
    = DeleteAccessKey {
        dakAccessKeyId :: Text
      -- ^ ID of the access key to be deleted.
      , dakUserName    :: Maybe Text
      -- ^ User name with which the access key is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery DeleteAccessKey where
    type ServiceConfiguration DeleteAccessKey = IamConfiguration
    signQuery DeleteAccessKey{..}
        = iamAction' "DeleteAccessKey" [
              Just ("AccessKeyId", dakAccessKeyId)
            , ("UserName",) <$> dakUserName
            ]

data DeleteAccessKeyResponse = DeleteAccessKeyResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer DeleteAccessKey DeleteAccessKeyResponse where
    type ResponseMetadata DeleteAccessKeyResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return DeleteAccessKeyResponse)

instance Transaction DeleteAccessKey DeleteAccessKeyResponse

instance AsMemoryResponse DeleteAccessKeyResponse where
    type MemoryResponse DeleteAccessKeyResponse = DeleteAccessKeyResponse
    loadToMemory = return
