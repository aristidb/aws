{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.DeleteUserPolicy
    ( DeleteUserPolicy(..)
    , DeleteUserPolicyResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text          (Text)
import           Data.Typeable

-- | Deletes the specified policy associated with the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUserPolicy.html>
data DeleteUserPolicy
    = DeleteUserPolicy {
        dupPolicyName :: Text
      -- ^ Name of the policy to be deleted.
      , dupUserName   :: Text
      -- ^ Name of the user with whom the policy is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery DeleteUserPolicy where
    type ServiceConfiguration DeleteUserPolicy = IamConfiguration
    signQuery DeleteUserPolicy{..}
        = iamAction "DeleteUserPolicy" [
              ("PolicyName", dupPolicyName)
            , ("UserName", dupUserName)
            ]

data DeleteUserPolicyResponse = DeleteUserPolicyResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer DeleteUserPolicy DeleteUserPolicyResponse where
    type ResponseMetadata DeleteUserPolicyResponse = IamMetadata
    responseConsumer _ = iamResponseConsumer (const $ return DeleteUserPolicyResponse)

instance Transaction DeleteUserPolicy DeleteUserPolicyResponse

instance AsMemoryResponse DeleteUserPolicyResponse where
    type MemoryResponse DeleteUserPolicyResponse = DeleteUserPolicyResponse
    loadToMemory = return
