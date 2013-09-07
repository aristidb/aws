{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.ListUsers
    ( ListUsers(..)
    , ListUsersResponse(..)
    , User(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable
import           Text.XML.Cursor     (laxElement, ($//), (&|))

-- | Lists users that have the specified path prefix.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUsers.html>
data ListUsers
    = ListUsers {
        luPathPrefix :: Maybe Text
      -- ^ Users defined under this path will be listed. If omitted, defaults
      -- to @/@, which lists all users.
      , luMarker     :: Maybe Text
      -- ^ Used for paginating requests. Marks the position of the last
      -- request.
      , luMaxItems   :: Maybe Integer
      -- ^ Used for paginating requests. Specifies the maximum number of items
      -- to return in the response. Defaults to 100.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery ListUsers where
    type ServiceConfiguration ListUsers = IamConfiguration
    signQuery ListUsers{..}
        = iamAction' "ListUsers" $ [
              ("PathPrefix",) <$> luPathPrefix
            ] <> markedIter luMarker luMaxItems

data ListUsersResponse
    = ListUsersResponse {
        lurUsers       :: [User]
      -- ^ List of 'User's.
      , lurIsTruncated :: Bool
      -- ^ @True@ if the request was truncated because of too many items.
      , lurMarker      :: Maybe Text
      -- ^ Marks the position at which the request was truncated. This value
      -- must be passed with the next request to continue listing from the
      -- last position.
      }
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer ListUsers ListUsersResponse where
    type ResponseMetadata ListUsersResponse = IamMetadata
    responseConsumer _
        = iamResponseConsumer $ \cursor -> do
            (lurIsTruncated, lurMarker) <- markedIterResponse cursor
            lurUsers <- sequence $
                cursor $// laxElement "member" &| parseUser
            return ListUsersResponse{..}

instance Transaction ListUsers ListUsersResponse

instance IteratedTransaction ListUsers ListUsersResponse where
    nextIteratedRequest request response
        = case lurMarker response of
            Nothing     -> Nothing
            Just marker -> Just $ request { luMarker = Just marker }

instance AsMemoryResponse ListUsersResponse where
    type MemoryResponse ListUsersResponse = ListUsersResponse
    loadToMemory = return
