{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.UpdateGroup
    ( UpdateGroup(..)
    , UpdateGroupResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable
import           Prelude

-- | Updates the name and/or path of the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateGroup.html>
data UpdateGroup
    = UpdateGroup {
        ugGroupName    :: Text
      -- ^ Name of the group to be updated.
      , ugNewGroupName :: Maybe Text
      -- ^ New name for the group.
      , ugNewPath     :: Maybe Text
      -- ^ New path to which the group will be moved.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery UpdateGroup where
    type ServiceConfiguration UpdateGroup = IamConfiguration
    signQuery UpdateGroup{..}
        = iamAction' "UpdateGroup" [
              Just ("GroupName", ugGroupName)
            , ("NewGroupName",) <$> ugNewGroupName
            , ("NewPath",) <$> ugNewPath
            ]

data UpdateGroupResponse = UpdateGroupResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer UpdateGroup UpdateGroupResponse where
    type ResponseMetadata UpdateGroupResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return UpdateGroupResponse)

instance Transaction UpdateGroup UpdateGroupResponse

instance AsMemoryResponse UpdateGroupResponse where
    type MemoryResponse UpdateGroupResponse = UpdateGroupResponse
    loadToMemory = return
