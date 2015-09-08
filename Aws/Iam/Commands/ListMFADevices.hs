{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
module Aws.Iam.Commands.ListMFADevices
       ( ListMFADevices(..)
       , ListMFADevicesResponse(..)
       ) where

import Aws.Core
import Aws.Iam.Core
import Aws.Iam.Internal
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

data ListMFADevices = ListMFADevices
                      { lmfaUserName :: Maybe Text
                      , lmfaMarker   :: Maybe Text
                        -- ^ Used for paginating requests. Marks the position of the last request.
                      , lmfaMaxItems :: Maybe Integer
                        -- ^ Used for paginating requests. Specifies the maximum number of items
                        -- to return in the response. Defaults to 100.
                      } deriving (Eq, Ord, Show, Typeable)

instance SignQuery ListMFADevices where
  type ServiceConfiguration ListMFADevices = IamConfiguration
  signQuery ListMFADevices{..} = iamAction' "ListMFADevices"
                                 ([ ("UserName",) <$> lmfaUserName ]
                                 <> markedIter lmfaMarker lmfaMaxItems)


data ListMFADevicesResponse = ListMFADevicesResponse
                              { lmfarMFADevices :: [MFADevice]
                              , lmfarIsTruncated :: Bool
                              , lmfarMarker :: Maybe Text
                              } deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer ListMFADevices ListMFADevicesResponse where
  type ResponseMetadata ListMFADevicesResponse = IamMetadata
  responseConsumer _req =
    iamResponseConsumer $ \ cursor -> do
      (lmfarIsTruncated, lmfarMarker) <- markedIterResponse cursor
      lmfarMFADevices <-
        sequence $ cursor $// laxElement "member" &| parseMFADevice
      return ListMFADevicesResponse{..}

instance Transaction ListMFADevices ListMFADevicesResponse

instance IteratedTransaction ListMFADevices ListMFADevicesResponse where
    nextIteratedRequest request response
        = case lmfarMarker response of
            Nothing     -> Nothing
            Just marker -> Just $ request { lmfaMarker = Just marker }

instance AsMemoryResponse ListMFADevicesResponse where
    type MemoryResponse ListMFADevicesResponse = ListMFADevicesResponse
    loadToMemory = return
