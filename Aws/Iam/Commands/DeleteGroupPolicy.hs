{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.DeleteGroupPolicy
    ( DeleteGroupPolicy(..)
    , DeleteGroupPolicyResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text          (Text)
import           Data.Typeable

-- | Deletes the specified policy associated with the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroupPolicy.html>
data DeleteGroupPolicy
    = DeleteGroupPolicy {
        dgpPolicyName :: Text
      -- ^ Name of the policy to be deleted.
      , dgpGroupName   :: Text
      -- ^ Name of the group with whom the policy is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery DeleteGroupPolicy where
    type ServiceConfiguration DeleteGroupPolicy = IamConfiguration
    signQuery DeleteGroupPolicy{..}
        = iamAction "DeleteGroupPolicy" [
              ("PolicyName", dgpPolicyName)
            , ("GroupName", dgpGroupName)
            ]

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer DeleteGroupPolicy DeleteGroupPolicyResponse where
    type ResponseMetadata DeleteGroupPolicyResponse = IamMetadata
    responseConsumer _ _ =
        iamResponseConsumer (const $ return DeleteGroupPolicyResponse)

instance Transaction DeleteGroupPolicy DeleteGroupPolicyResponse

instance AsMemoryResponse DeleteGroupPolicyResponse where
    type MemoryResponse DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    loadToMemory = return
