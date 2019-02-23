{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.PutGroupPolicy
    ( PutGroupPolicy(..)
    , PutGroupPolicyResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text        (Text)
import           Data.Typeable

-- | Adds a policy document with the specified name, associated with the
-- specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutGroupPolicy.html>
data PutGroupPolicy
    = PutGroupPolicy {
        pgpPolicyDocument :: Text
      -- ^ The policy document.
      , pgpPolicyName     :: Text
      -- ^ Name of the policy.
      , pgpGroupName       :: Text
      -- ^ Name of the group with whom this policy is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery PutGroupPolicy where
    type ServiceConfiguration PutGroupPolicy = IamConfiguration
    signQuery PutGroupPolicy{..}
        = iamAction "PutGroupPolicy" [
              ("PolicyDocument", pgpPolicyDocument)
            , ("PolicyName"    , pgpPolicyName)
            , ("GroupName"      , pgpGroupName)
            ]

data PutGroupPolicyResponse = PutGroupPolicyResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer PutGroupPolicy PutGroupPolicyResponse where
    type ResponseMetadata PutGroupPolicyResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return PutGroupPolicyResponse)

instance Transaction PutGroupPolicy PutGroupPolicyResponse

instance AsMemoryResponse PutGroupPolicyResponse where
    type MemoryResponse PutGroupPolicyResponse = PutGroupPolicyResponse
    loadToMemory = return
