{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.GetUser
    ( GetUser(..)
    , GetUserResponse(..)
    , User(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable

-- | Retreives information about the given user.
--
-- If a user name is not given, IAM determines the user name based on the
-- access key signing the request.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUser.html>
data GetUser = GetUser (Maybe Text)
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery GetUser where
    type ServiceConfiguration GetUser = IamConfiguration
    signQuery (GetUser user)
        = iamAction' "GetUser" [("UserName",) <$> user]

data GetUserResponse = GetUserResponse User
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer GetUser GetUserResponse where
    type ResponseMetadata GetUserResponse = IamMetadata
    responseConsumer _ _ = iamResponseConsumer $
                           fmap GetUserResponse . parseUser

instance Transaction GetUser GetUserResponse

instance AsMemoryResponse GetUserResponse where
    type MemoryResponse GetUserResponse = GetUserResponse
    loadToMemory = return
