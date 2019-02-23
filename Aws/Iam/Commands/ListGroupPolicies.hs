{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.ListGroupPolicies
    ( ListGroupPolicies(..)
    , ListGroupPoliciesResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text        (Text)
import           Data.Typeable
import           Text.XML.Cursor  (content, laxElement, ($//), (&/))

-- | Lists the group policies associated with the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupPolicies.html>
data ListGroupPolicies
    = ListGroupPolicies {
        lgpGroupName :: Text
      -- ^ Policies associated with this group will be listed.
      , lgpMarker   :: Maybe Text
      -- ^ Used for paginating requests. Marks the position of the last
      -- request.
      , lgpMaxItems :: Maybe Integer
      -- ^ Used for paginating requests. Specifies the maximum number of items
      -- to return in the response. Defaults to 100.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery ListGroupPolicies where
    type ServiceConfiguration ListGroupPolicies = IamConfiguration
    signQuery ListGroupPolicies{..}
        = iamAction' "ListGroupPolicies" $ [
              Just ("GroupName", lgpGroupName)
            ] <> markedIter lgpMarker lgpMaxItems

data ListGroupPoliciesResponse
    = ListGroupPoliciesResponse {
        lgprPolicyNames :: [Text]
      -- ^ List of policy names.
      , lgprIsTruncated :: Bool
      -- ^ @True@ if the request was truncated because of too many items.
      , lgprMarker      :: Maybe Text
      -- ^ Marks the position at which the request was truncated. This value
      -- must be passed with the next request to continue listing from the
      -- last position.
      }
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer ListGroupPolicies ListGroupPoliciesResponse where
    type ResponseMetadata ListGroupPoliciesResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer $ \cursor -> do
            (lgprIsTruncated, lgprMarker) <- markedIterResponse cursor
            let lgprPolicyNames = cursor $// laxElement "member" &/ content
            return ListGroupPoliciesResponse{..}

instance Transaction ListGroupPolicies ListGroupPoliciesResponse

instance IteratedTransaction ListGroupPolicies ListGroupPoliciesResponse where
    nextIteratedRequest request response
        = case lgprMarker response of
            Nothing     -> Nothing
            Just marker -> Just $ request { lgpMarker = Just marker }

instance AsMemoryResponse ListGroupPoliciesResponse where
    type MemoryResponse ListGroupPoliciesResponse = ListGroupPoliciesResponse
    loadToMemory = return
