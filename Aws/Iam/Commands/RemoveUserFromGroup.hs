{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.RemoveUserFromGroup
    ( RemoveUserFromGroup(..)
    , RemoveUserFromGroupResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text        (Text)
import           Data.Typeable

-- | Removes the specified user from the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveUserFromGroup.html>
data RemoveUserFromGroup
    = RemoveUserFromGroup {
        rufgGroupName :: Text
      -- ^ Name of the group to update.
      , rufgUserName  :: Text
      -- ^ The of the user to add.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery RemoveUserFromGroup where
    type ServiceConfiguration RemoveUserFromGroup = IamConfiguration
    signQuery RemoveUserFromGroup{..}
        = iamAction "RemoveUserFromGroup" [
              ("GroupName"     , rufgGroupName)
            , ("UserName"      , rufgUserName)
            ]

data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer RemoveUserFromGroup RemoveUserFromGroupResponse where
    type ResponseMetadata RemoveUserFromGroupResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return RemoveUserFromGroupResponse)

instance Transaction RemoveUserFromGroup RemoveUserFromGroupResponse

instance AsMemoryResponse RemoveUserFromGroupResponse where
    type MemoryResponse RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    loadToMemory = return
