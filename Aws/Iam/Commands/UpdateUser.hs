{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.UpdateUser
    ( UpdateUser(..)
    , UpdateUserResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable

-- | Updates the name and/or path of the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateUser.html>
data UpdateUser
    = UpdateUser {
        uuUserName    :: Text
      -- ^ Name of the user to be updated.
      , uuNewUserName :: Maybe Text
      -- ^ New name for the user.
      , uuNewPath     :: Maybe Text
      -- ^ New path to which the user will be moved.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery UpdateUser where
    type ServiceConfiguration UpdateUser = IamConfiguration
    signQuery UpdateUser{..}
        = iamAction' "UpdateUser" [
              Just ("UserName", uuUserName)
            , ("NewUserName",) <$> uuNewUserName
            , ("NewPath",) <$> uuNewPath
            ]

data UpdateUserResponse = UpdateUserResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer UpdateUser UpdateUserResponse where
    type ResponseMetadata UpdateUserResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer (const $ return UpdateUserResponse)

instance Transaction UpdateUser UpdateUserResponse

instance AsMemoryResponse UpdateUserResponse where
    type MemoryResponse UpdateUserResponse = UpdateUserResponse
    loadToMemory = return
