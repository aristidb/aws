{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.Iam.Commands.ListAccessKeys
    ( ListAccessKeys(..)
    , ListAccessKeysResponse(..)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Aws.Iam.Internal
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Time
import           Data.Typeable
import           Text.XML.Cursor     (laxElement, ($/), ($//), (&|))

-- | Returns the access keys associated with the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccessKeys.html>
data ListAccessKeys
    = ListAccessKeys {
        lakUserName :: Maybe Text
      -- ^ Name of the user. If the user name is not specified, IAM will
      -- determine the user based on the key sigining the request.
      , lakMarker   :: Maybe Text
      -- ^ Used for paginating requests. Marks the position of the last
      -- request.
      , lakMaxItems :: Maybe Integer
      -- ^ Used for paginating requests. Specifies the maximum number of items
      -- to return in the response. Defaults to 100.
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery ListAccessKeys where
    type ServiceConfiguration ListAccessKeys = IamConfiguration
    signQuery ListAccessKeys{..}
        = iamAction' "ListAccessKeys" $ [
              ("UserName",) <$> lakUserName
            ] <> markedIter lakMarker lakMaxItems

-- | Represents the IAM @AccessKeyMetadata@ data type.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AccessKeyMetadata.html>
data AccessKeyMetadata
    = AccessKeyMetadata {
        akmAccessKeyId :: Maybe Text
      -- ^ ID of the access key.
      , akmCreateDate  :: Maybe UTCTime
      -- ^ Date and time at which the access key was created.
      , akmStatus      :: Maybe Text
      -- ^ Whether the access key is active.
      , akmUserName    :: Maybe Text
      -- ^ Name of the user with whom the access key is associated.
      }
    deriving (Eq, Ord, Show, Typeable)

data ListAccessKeysResponse
    = ListAccessKeysResponse {
        lakrAccessKeyMetadata :: [AccessKeyMetadata]
      -- ^ List of 'AccessKeyMetadata' objects
      , lakrIsTruncated       :: Bool
      -- ^ @True@ if the request was truncated because of too many items.
      , lakrMarker            :: Maybe Text
      -- ^ Marks the position at which the request was truncated. This value
      -- must be passed with the next request to continue listing from the
      -- last position.
      }
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer ListAccessKeys ListAccessKeysResponse where
    type ResponseMetadata ListAccessKeysResponse = IamMetadata
    responseConsumer _ _
        = iamResponseConsumer $ \cursor -> do
            (lakrIsTruncated, lakrMarker) <- markedIterResponse cursor
            lakrAccessKeyMetadata <- sequence $
                cursor $// laxElement "member" &| buildAKM
            return ListAccessKeysResponse{..}
        where
            buildAKM m = do
                let mattr name = mhead $ m $/ elContent name
                let akmAccessKeyId = mattr "AccessKeyId"
                    akmStatus      = mattr "Status"
                    akmUserName    = mattr "UserName"
                akmCreateDate <- case m $/ elCont "CreateDate" of
                                    (x:_) -> Just <$> parseDateTime x
                                    _     -> return Nothing
                return AccessKeyMetadata{..}

            mhead (x:_) = Just x
            mhead  _    = Nothing

instance Transaction ListAccessKeys ListAccessKeysResponse

instance IteratedTransaction ListAccessKeys ListAccessKeysResponse where
    nextIteratedRequest request response
        = case lakrMarker response of
            Nothing     -> Nothing
            Just marker -> Just $ request { lakMarker = Just marker }

instance AsMemoryResponse ListAccessKeysResponse where
    type MemoryResponse ListAccessKeysResponse = ListAccessKeysResponse
    loadToMemory = return
