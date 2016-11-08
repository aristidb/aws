{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.CreateUser
    ( CreateUser(..)
    , CreateUserResponse(..)
    , User(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable
import           Prelude

-- | Creates a new user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateUser.html>
data CreateUser
    = CreateUser {
        cuUserName :: Text
      -- ^ Name of the new user
      , cuPath     :: Maybe Text
      -- ^ Path under which the user will be created. Defaults to @/@ if
      -- omitted.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery CreateUser where
    type ServiceConfiguration CreateUser = IamConfiguration
    signQuery CreateUser{..}
        = iamAction' "CreateUser" [
              Just ("UserName", cuUserName)
            , ("Path",) <$> cuPath
            ]

data CreateUserResponse = CreateUserResponse User
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer CreateUser CreateUserResponse where
    type ResponseMetadata CreateUserResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer $
          fmap CreateUserResponse . parseUser

instance Transaction CreateUser CreateUserResponse

instance AsMemoryResponse CreateUserResponse where
    type MemoryResponse CreateUserResponse = CreateUserResponse
    loadToMemory = return
