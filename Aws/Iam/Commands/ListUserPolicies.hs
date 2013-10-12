{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.ListUserPolicies
    ( ListUserPolicies(..)
    , ListUserPoliciesResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Data.Text        (Text)
import           Data.Typeable
import           Text.XML.Cursor  (content, laxElement, ($//), (&/))

-- | Lists the user policies associated with the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUserPolicies.html>
data ListUserPolicies
    = ListUserPolicies {
        lupUserName :: Text
      -- ^ Policies associated with this user will be listed.
      , lupMarker   :: Maybe Text
      -- ^ Used for paginating requests. Marks the position of the last
      -- request.
      , lupMaxItems :: Maybe Integer
      -- ^ Used for paginating requests. Specifies the maximum number of items
      -- to return in the response. Defaults to 100.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery ListUserPolicies where
    type ServiceConfiguration ListUserPolicies = IamConfiguration
    signQuery ListUserPolicies{..}
        = iamAction' "ListUserPolicies" $ [
              Just ("UserName", lupUserName)
            ] <> markedIter lupMarker lupMaxItems

data ListUserPoliciesResponse
    = ListUserPoliciesResponse {
        luprPolicyNames :: [Text]
      -- ^ List of policy names.
      , luprIsTruncated :: Bool
      -- ^ @True@ if the request was truncated because of too many items.
      , luprMarker      :: Maybe Text
      -- ^ Marks the position at which the request was truncated. This value
      -- must be passed with the next request to continue listing from the
      -- last position.
      }
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer ListUserPolicies ListUserPoliciesResponse where
    type ResponseMetadata ListUserPoliciesResponse = IamMetadata
    responseConsumer _
        = iamResponseConsumer $ \cursor -> do
            (luprIsTruncated, luprMarker) <- markedIterResponse cursor
            let luprPolicyNames = cursor $// laxElement "member" &/ content
            return ListUserPoliciesResponse{..}

instance Transaction ListUserPolicies ListUserPoliciesResponse

instance IteratedTransaction ListUserPolicies ListUserPoliciesResponse where
    nextIteratedRequest request response
        = case luprMarker response of
            Nothing     -> Nothing
            Just marker -> Just $ request { lupMarker = Just marker }

instance AsMemoryResponse ListUserPoliciesResponse where
    type MemoryResponse ListUserPoliciesResponse = ListUserPoliciesResponse
    loadToMemory = return
