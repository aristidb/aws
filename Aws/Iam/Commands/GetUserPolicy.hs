{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.GetUserPolicy
    ( GetUserPolicy(..)
    , GetUserPolicyResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Data.Typeable
import qualified Network.HTTP.Types  as HTTP
import           Text.XML.Cursor     (($//))

-- | Retreives the specified policy document for the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUserPolicy.html>
data GetUserPolicy
    = GetUserPolicy {
        gupPolicyName :: Text
      -- ^ Name of the policy.
      , gupUserName   :: Text
      -- ^ Name of the user with whom the policy is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery GetUserPolicy where
    type ServiceConfiguration GetUserPolicy = IamConfiguration
    signQuery GetUserPolicy{..}
        = iamAction "GetUserPolicy" [
              ("PolicyName", gupPolicyName)
            , ("UserName", gupUserName)
            ]

data GetUserPolicyResponse
    = GetUserPolicyResponse {
        guprPolicyDocument :: Text
      -- ^ The policy document.
      , guprPolicyName     :: Text
      -- ^ Name of the policy.
      , guprUserName       :: Text
      -- ^ Name of the user with whom the policy is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer GetUserPolicy GetUserPolicyResponse where
    type ResponseMetadata GetUserPolicyResponse = IamMetadata
    responseConsumer _
        = iamResponseConsumer $ \cursor -> do
            let attr name = force ("Missing " ++ Text.unpack name) $
                            cursor $// elContent name
            guprPolicyDocument <- decodePolicy <$>
                                  attr "PolicyDocument"
            guprPolicyName     <- attr "PolicyName"
            guprUserName       <- attr "UserName"
            return GetUserPolicyResponse{..}
        where
          decodePolicy = Text.decodeUtf8 . HTTP.urlDecode False
                       . Text.encodeUtf8


instance Transaction GetUserPolicy GetUserPolicyResponse

instance AsMemoryResponse GetUserPolicyResponse where
    type MemoryResponse GetUserPolicyResponse = GetUserPolicyResponse
    loadToMemory = return
