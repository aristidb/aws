-- ------------------------------------------------------ --
-- Copyright © 2012 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-}  

-- | GET GetChange
--
--   Returns the current status of change batch request.
--
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_GetChange.html>
--
module Aws.Route53.Commands.GetChange where

import           Aws.Core
import           Aws.Route53.Core
import qualified Data.Text.Encoding         as T

data GetChange = GetChange
               { changeId :: ChangeId
               } deriving (Show)

data GetChangeResponse = GetChangeResponse
                       { gcrChangeInfo :: ChangeInfo
                       } deriving (Show)

getChange :: ChangeId -> GetChange
getChange changeId = GetChange changeId

-- | ServiceConfiguration: 'Route53Configuration'
instance SignQuery GetChange where
    type ServiceConfiguration GetChange = Route53Configuration
    signQuery GetChange{..} = route53SignQuery method resource query body
      where
      method = Get
      resource = T.encodeUtf8 . qualifiedIdText $ changeId
      query = []
      body = Nothing

instance ResponseConsumer r GetChangeResponse where
    type ResponseMetadata GetChangeResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "GetChangeResponse" cursor
            changeInfo <- r53Parse cursor
            return $ GetChangeResponse changeInfo

instance Transaction GetChange GetChangeResponse

