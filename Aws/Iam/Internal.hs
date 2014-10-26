{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module Aws.Iam.Internal
    ( iamAction
    , iamAction'
    , markedIter
    , markedIterResponse

    -- * Re-exports
    , (<>)
    ) where

import           Aws.Core
import           Aws.Iam.Core
import           Control.Applicative
import           Control.Arrow       (second)
import           Control.Monad
import           Control.Monad.Catch (MonadThrow)
import           Data.ByteString     (ByteString)
import           Data.Maybe
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Text.XML.Cursor     (($//))
import qualified Text.XML.Cursor     as Cu

-- | Similar to 'iamSignQuery'. Accepts parameters in @Text@ form and UTF-8
-- encodes them. Accepts the @Action@ parameter separately since it's always
-- required.
iamAction
    :: ByteString
    -> [(ByteString, Text)]
    -> IamConfiguration qt
    -> SignatureData
    -> SignedQuery
iamAction action = iamSignQuery
                 . (:) ("Action", action)
                 . map (second Text.encodeUtf8)

-- | Similar to 'iamAction'. Accepts parameter list with @Maybe@ parameters.
-- Ignores @Nothing@s.
iamAction'
    :: ByteString
    -> [Maybe (ByteString, Text)]
    -> IamConfiguration qt
    -> SignatureData
    -> SignedQuery
iamAction' action = iamAction action . catMaybes

-- | Returns the parameters @Marker@ and @MaxItems@ that are present in all
-- IAM data pagination requests.
markedIter :: Maybe Text -> Maybe Integer -> [Maybe (ByteString, Text)]
markedIter marker maxItems
    = [ ("Marker"  ,)                 <$> marker
      , ("MaxItems",) . encodeInteger <$> maxItems
      ]
  where
    encodeInteger = Text.pack . show

-- | Reads and returns the @IsTruncated@ and @Marker@ attributes present in
-- all IAM data pagination responses.
markedIterResponse
    :: MonadThrow m
    => Cu.Cursor
    -> m (Bool, Maybe Text)
markedIterResponse cursor = do
    isTruncated <- (Text.toCaseFold "true" ==) `liftM` attr "IsTruncated"
    marker      <- if isTruncated
                    then Just `liftM` attr "Marker"
                    else return Nothing
    return (isTruncated, marker)
  where
    attr name = force ("Missing " ++ Text.unpack name) $
                cursor $// elContent name
