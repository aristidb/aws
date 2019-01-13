{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.ListGroups
    ( ListGroups(..)
    , ListGroupsResponse(..)
    , Group(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Typeable
import           Prelude
import           Text.XML.Cursor     (laxElement, ($//), (&|))

-- | Lists groups that have the specified path prefix.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroups.html>
data ListGroups
    = ListGroups {
        lgPathPrefix :: Maybe Text
      -- ^ Groups defined under this path will be listed. If omitted, defaults
      -- to @/@, which lists all groups.
      , lgMarker     :: Maybe Text
      -- ^ Used for paginating requests. Marks the position of the last
      -- request.
      , lgMaxItems   :: Maybe Integer
      -- ^ Used for paginating requests. Specifies the maximum number of items
      -- to return in the response. Defaults to 100.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery ListGroups where
    type ServiceConfiguration ListGroups = IamConfiguration
    signQuery ListGroups{..}
        = iamAction' "ListGroups" $ [
              ("PathPrefix",) <$> lgPathPrefix
            ] <> markedIter lgMarker lgMaxItems

data ListGroupsResponse
    = ListGroupsResponse {
        lgrGroups       :: [Group]
      -- ^ List of 'Group's.
      , lgrIsTruncated :: Bool
      -- ^ @True@ if the request was truncated because of too many items.
      , lgrMarker      :: Maybe Text
      -- ^ Marks the position at which the request was truncated. This value
      -- must be passed with the next request to continue listing from the
      -- last position.
      }
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer ListGroups ListGroupsResponse where
    type ResponseMetadata ListGroupsResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer $ \cursor -> do
            (lgrIsTruncated, lgrMarker) <- markedIterResponse cursor
            lgrGroups <- sequence $
                cursor $// laxElement "member" &| parseGroup
            return ListGroupsResponse{..}

instance Transaction ListGroups ListGroupsResponse

instance IteratedTransaction ListGroups ListGroupsResponse where
    nextIteratedRequest request response
        = case lgrMarker response of
            Nothing     -> Nothing
            Just marker -> Just $ request { lgMarker = Just marker }

instance AsMemoryResponse ListGroupsResponse where
    type MemoryResponse ListGroupsResponse = ListGroupsResponse
    loadToMemory = return
