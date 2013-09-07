{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.DeleteUser
    ( DeleteUser(..)
    , DeleteUserResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text          (Text)
import           Data.Typeable

-- | Deletes the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUser.html>
data DeleteUser = DeleteUser Text
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery DeleteUser where
    type ServiceConfiguration DeleteUser = IamConfiguration
    signQuery (DeleteUser userName)
        = iamAction "DeleteUser" [("UserName", userName)]

data DeleteUserResponse = DeleteUserResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer DeleteUser DeleteUserResponse where
    type ResponseMetadata DeleteUserResponse = IamMetadata
    responseConsumer _ = iamResponseConsumer (const $ return DeleteUserResponse)

instance Transaction DeleteUser DeleteUserResponse

instance AsMemoryResponse DeleteUserResponse where
    type MemoryResponse DeleteUserResponse = DeleteUserResponse
    loadToMemory = return
