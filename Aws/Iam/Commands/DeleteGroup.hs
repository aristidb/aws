{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.DeleteGroup
    ( DeleteGroup(..)
    , DeleteGroupResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text          (Text)
import           Data.Typeable

-- | Deletes the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroup.html>
data DeleteGroup = DeleteGroup Text
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery DeleteGroup where
    type ServiceConfiguration DeleteGroup = IamConfiguration
    signQuery (DeleteGroup groupName)
        = iamAction "DeleteGroup" [("GroupName", groupName)]

data DeleteGroupResponse = DeleteGroupResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer DeleteGroup DeleteGroupResponse where
    type ResponseMetadata DeleteGroupResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return DeleteGroupResponse)

instance Transaction DeleteGroup DeleteGroupResponse

instance AsMemoryResponse DeleteGroupResponse where
    type MemoryResponse DeleteGroupResponse = DeleteGroupResponse
    loadToMemory = return
