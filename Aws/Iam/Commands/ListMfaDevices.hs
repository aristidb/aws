{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
module Aws.Iam.Commands.ListMfaDevices
       ( ListMfaDevices(..)
       , ListMfaDevicesResponse(..)
       ) where

import Aws.Core
import Aws.Iam.Core
import Aws.Iam.Internal
import Control.Applicative
import Data.Text (Text)
import Data.Typeable
import Text.XML.Cursor (laxElement, ($//), (&|))
-- | Lists the MFA devices. If the request includes the user name,
-- then this action lists all the MFA devices associated with the
-- specified user name. If you do not specify a user name, IAM
-- determines the user name implicitly based on the AWS access key ID
-- signing the request.
--
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_ListMFADevices.html>

data ListMfaDevices = ListMfaDevices
                      { lmfaUserName :: Maybe Text
                        -- ^ The name of the user whose MFA devices
                        -- you want to list.  If you do not specify a
                        -- user name, IAM determines the user name
                        -- implicitly based on the AWS access key ID
                        -- signing the request
                      , lmfaMarker   :: Maybe Text
                        -- ^ Used for paginating requests. Marks the
                        -- position of the last request.
                      , lmfaMaxItems :: Maybe Integer
                        -- ^ Used for paginating requests. Specifies
                        -- the maximum number of items to return in
                        -- the response. Defaults to 100.
                      } deriving (Eq, Ord, Show, Typeable)

instance SignQuery ListMfaDevices where
  type ServiceConfiguration ListMfaDevices = IamConfiguration
  signQuery ListMfaDevices{..} = iamAction' "ListMFADevices"
                                 ([ ("UserName",) <$> lmfaUserName ]
                                 <> markedIter lmfaMarker lmfaMaxItems)

data ListMfaDevicesResponse = ListMfaDevicesResponse
                              { lmfarMfaDevices :: [MfaDevice]
                                -- ^ List of 'MFA Device's.
                              , lmfarIsTruncated :: Bool
                                -- ^ @True@ if the request was
                                -- truncated because of too many
                                -- items.
                              , lmfarMarker :: Maybe Text
                                -- ^ Marks the position at which the
                                -- request was truncated. This value
                                -- must be passed with the next
                                -- request to continue listing from
                                -- the last position.
                              } deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer ListMfaDevices ListMfaDevicesResponse where
  type ResponseMetadata ListMfaDevicesResponse = IamMetadata
  responseConsumer _req =
    iamResponseConsumer $ \ cursor -> do
      (lmfarIsTruncated, lmfarMarker) <- markedIterResponse cursor
      lmfarMfaDevices <-
        sequence $ cursor $// laxElement "member" &| parseMfaDevice
      return ListMfaDevicesResponse{..}

instance Transaction ListMfaDevices ListMfaDevicesResponse

instance IteratedTransaction ListMfaDevices ListMfaDevicesResponse where
    nextIteratedRequest request response
        = case lmfarMarker response of
            Nothing     -> Nothing
            Just marker -> Just $ request { lmfaMarker = Just marker }

instance AsMemoryResponse ListMfaDevicesResponse where
    type MemoryResponse ListMfaDevicesResponse = ListMfaDevicesResponse
    loadToMemory = return
