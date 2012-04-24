{-# LANGUAGE 
    RecordWildCards
  , TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TupleSections 
  #-}

-- | GET GetChange
--
--   Returns the current status of change batch request.
--
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_GetChange.html>
--
module Aws.Route53.Commands.GetChange where

import           Aws.Response
import           Aws.Signature
import           Aws.Route53.Info
import           Aws.Route53.Model
import           Aws.Route53.Metadata
import           Aws.Route53.Query
import           Aws.Route53.Response
import           Aws.Transaction
import           Aws.Http                   (Method(..))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString            as B

data GetChange = GetChange
               { changeId :: T.Text
               } deriving (Show)

data GetChangeResponse = GetChangeResponse
                       { gcrChangeInfo :: ChangeInfo
                       } deriving (Show)

getChange :: T.Text -> GetChange
getChange changeId = GetChange changeId

instance SignQuery GetChange where
    type Info GetChange = Route53Info
    signQuery GetChange{..} = route53SignQuery method resource query
      where
      method = Get
      resource = "/change/" `B.append` (T.encodeUtf8 changeId)
      query = []

instance ResponseConsumer r GetChangeResponse where
    type ResponseMetadata GetChangeResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "GetChangeResponse" cursor
            changeInfo <- r53Parse cursor
            return $ GetChangeResponse changeInfo

instance Transaction GetChange GetChangeResponse where

