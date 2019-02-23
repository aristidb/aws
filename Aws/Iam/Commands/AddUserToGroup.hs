{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.AddUserToGroup
    ( AddUserToGroup(..)
    , AddUserToGroupResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text        (Text)
import           Data.Typeable

-- | Adds the specified user to the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddUserToGroup.html>
data AddUserToGroup
    = AddUserToGroup {
        autgGroupName :: Text
      -- ^ Name of the group to update.
      , autgUserName  :: Text
      -- ^ The of the user to add.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery AddUserToGroup where
    type ServiceConfiguration AddUserToGroup = IamConfiguration
    signQuery AddUserToGroup{..}
        = iamAction "AddUserToGroup" [
              ("GroupName"     , autgGroupName)
            , ("UserName"      , autgUserName)
            ]

data AddUserToGroupResponse = AddUserToGroupResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer AddUserToGroup AddUserToGroupResponse where
    type ResponseMetadata AddUserToGroupResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return AddUserToGroupResponse)

instance Transaction AddUserToGroup AddUserToGroupResponse

instance AsMemoryResponse AddUserToGroupResponse where
    type MemoryResponse AddUserToGroupResponse = AddUserToGroupResponse
    loadToMemory = return
