{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.GetGroupPolicy
    ( GetGroupPolicy(..)
    , GetGroupPolicyResponse(..)
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
import           Prelude

-- | Retreives the specified policy document for the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroupPolicy.html>
data GetGroupPolicy
    = GetGroupPolicy {
        ggpPolicyName :: Text
      -- ^ Name of the policy.
      , ggpGroupName   :: Text
      -- ^ Name of the group with whom the policy is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery GetGroupPolicy where
    type ServiceConfiguration GetGroupPolicy = IamConfiguration
    signQuery GetGroupPolicy{..}
        = iamAction "GetGroupPolicy" [
              ("PolicyName", ggpPolicyName)
            , ("GroupName", ggpGroupName)
            ]

data GetGroupPolicyResponse
    = GetGroupPolicyResponse {
        ggprPolicyDocument :: Text
      -- ^ The policy document.
      , ggprPolicyName     :: Text
      -- ^ Name of the policy.
      , ggprGroupName       :: Text
      -- ^ Name of the group with whom the policy is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer GetGroupPolicy GetGroupPolicyResponse where
    type ResponseMetadata GetGroupPolicyResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer $ \cursor -> do
            let attr name = force ("Missing " ++ Text.unpack name) $
                            cursor $// elContent name
            ggprPolicyDocument <- decodePolicy <$>
                                  attr "PolicyDocument"
            ggprPolicyName     <- attr "PolicyName"
            ggprGroupName       <- attr "GroupName"
            return GetGroupPolicyResponse{..}
        where
          decodePolicy = Text.decodeUtf8 . HTTP.urlDecode False
                       . Text.encodeUtf8


instance Transaction GetGroupPolicy GetGroupPolicyResponse

instance AsMemoryResponse GetGroupPolicyResponse where
    type MemoryResponse GetGroupPolicyResponse = GetGroupPolicyResponse
    loadToMemory = return
