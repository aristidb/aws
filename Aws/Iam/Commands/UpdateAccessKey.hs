{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.UpdateAccessKey
    ( UpdateAccessKey(..)
    , UpdateAccessKeyResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable
import           Prelude

-- | Changes the status of the specified access key.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccessKey.html>
data UpdateAccessKey
    = UpdateAccessKey {
        uakAccessKeyId :: Text
      -- ^ ID of the access key to update.
      , uakStatus      :: AccessKeyStatus
      -- ^ New status of the access key.
      , uakUserName    :: Maybe Text
      -- ^ Name of the user to whom the access key belongs. If omitted, the
      -- user will be determined based on the access key used to sign the
      -- request.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery UpdateAccessKey where
    type ServiceConfiguration UpdateAccessKey = IamConfiguration
    signQuery UpdateAccessKey{..}
        = iamAction' "UpdateAccessKey" [
              Just ("AccessKeyId", uakAccessKeyId)
            , Just ("Status", showStatus uakStatus)
            , ("UserName",) <$> uakUserName
            ]
        where
          showStatus AccessKeyActive = "Active"
          showStatus _               = "Inactive"

data UpdateAccessKeyResponse = UpdateAccessKeyResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer UpdateAccessKey UpdateAccessKeyResponse where
    type ResponseMetadata UpdateAccessKeyResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return UpdateAccessKeyResponse)

instance Transaction UpdateAccessKey UpdateAccessKeyResponse

instance AsMemoryResponse UpdateAccessKeyResponse where
    type MemoryResponse UpdateAccessKeyResponse = UpdateAccessKeyResponse
    loadToMemory = return
