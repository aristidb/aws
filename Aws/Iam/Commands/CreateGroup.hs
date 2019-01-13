{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.CreateGroup
    ( CreateGroup(..)
    , CreateGroupResponse(..)
    , Group(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable
import           Prelude

-- | Creates a new group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateGroup.html>
data CreateGroup
    = CreateGroup {
        cgGroupName :: Text
      -- ^ Name of the new group
      , cgPath     :: Maybe Text
      -- ^ Path under which the group will be created. Defaults to @/@ if
      -- omitted.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery CreateGroup where
    type ServiceConfiguration CreateGroup = IamConfiguration
    signQuery CreateGroup{..}
        = iamAction' "CreateGroup" [
              Just ("GroupName", cgGroupName)
            , ("Path",) <$> cgPath
            ]

data CreateGroupResponse = CreateGroupResponse Group
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer CreateGroup CreateGroupResponse where
    type ResponseMetadata CreateGroupResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer $
          fmap CreateGroupResponse . parseGroup

instance Transaction CreateGroup CreateGroupResponse

instance AsMemoryResponse CreateGroupResponse where
    type MemoryResponse CreateGroupResponse = CreateGroupResponse
    loadToMemory = return
