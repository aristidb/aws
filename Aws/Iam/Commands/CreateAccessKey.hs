{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.CreateAccessKey
    ( CreateAccessKey(..)
    , CreateAccessKeyResponse(..)
    , AccessKey(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Time
import           Data.Typeable
import           Text.XML.Cursor     (($//))

-- | Creates a new AWS secret access key and corresponding AWS access key ID
-- for the given user name.
--
-- If a user name is not provided, IAM will determine the user name based on
-- the access key signing the request.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateAccessKey.html>
data CreateAccessKey = CreateAccessKey (Maybe Text)
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery CreateAccessKey where
    type ServiceConfiguration CreateAccessKey = IamConfiguration
    signQuery (CreateAccessKey user)
        = iamAction' "CreateAccessKey" [("UserName",) <$> user]

-- | Represents the IAM @AccessKey@ data type.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AccessKey.html>
data AccessKey
    = AccessKey {
        akAccessKeyId     :: Text
      -- ^ The Access Key ID.
      , akCreateDate      :: Maybe UTCTime
      -- ^ Date and time at which the access key was created.
      , akSecretAccessKey :: Text
      -- ^ Secret key used to sign requests. The secret key is accessible only
      -- during key creation.
      , akStatus          :: AccessKeyStatus
      -- ^ Whether the access key is active or not.
      , akUserName        :: Text
      -- ^ The user name for which this key is defined.
      }
    deriving (Eq, Ord, Show, Typeable)

data CreateAccessKeyResponse
    = CreateAccessKeyResponse AccessKey
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer CreateAccessKey CreateAccessKeyResponse where
    type ResponseMetadata CreateAccessKeyResponse = IamMetadata
    responseConsumer _
        = iamResponseConsumer $ \cursor -> do
            let attr name = force ("Missing " ++ Text.unpack name) $
                            cursor $// elContent name
            akAccessKeyId     <- attr "AccessKeyId"
            akSecretAccessKey <- attr "SecretAccessKey"
            akStatus          <- readAccessKeyStatus <$> attr "Status"
            akUserName        <- attr "UserName"
            akCreateDate      <- readDate cursor
            return $ CreateAccessKeyResponse AccessKey{..}
        where
          readDate c = case c $// elCont "CreateDate" of
                        (x:_) -> Just <$> parseDateTime x
                        _     -> return Nothing
          readAccessKeyStatus s
              | Text.toCaseFold s == "Active" = AccessKeyActive
              | otherwise                     = AccessKeyInactive


instance Transaction CreateAccessKey CreateAccessKeyResponse

instance AsMemoryResponse CreateAccessKeyResponse where
    type MemoryResponse CreateAccessKeyResponse = CreateAccessKeyResponse
    loadToMemory = return
