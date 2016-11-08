{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.PutUserPolicy
    ( PutUserPolicy(..)
    , PutUserPolicyResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text        (Text)
import           Data.Typeable

-- | Adds a policy document with the specified name, associated with the
-- specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutUserPolicy.html>
data PutUserPolicy
    = PutUserPolicy {
        pupPolicyDocument :: Text
      -- ^ The policy document.
      , pupPolicyName     :: Text
      -- ^ Name of the policy.
      , pupUserName       :: Text
      -- ^ Name of the user with whom this policy is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery PutUserPolicy where
    type ServiceConfiguration PutUserPolicy = IamConfiguration
    signQuery PutUserPolicy{..}
        = iamAction "PutUserPolicy" [
              ("PolicyDocument", pupPolicyDocument)
            , ("PolicyName"    , pupPolicyName)
            , ("UserName"      , pupUserName)
            ]

data PutUserPolicyResponse = PutUserPolicyResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer PutUserPolicy PutUserPolicyResponse where
    type ResponseMetadata PutUserPolicyResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return PutUserPolicyResponse)

instance Transaction PutUserPolicy PutUserPolicyResponse

instance AsMemoryResponse PutUserPolicyResponse where
    type MemoryResponse PutUserPolicyResponse = PutUserPolicyResponse
    loadToMemory = return
