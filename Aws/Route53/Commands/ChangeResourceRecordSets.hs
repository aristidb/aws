{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-} 

-- | POST ChangeResourceRecordSetrs
--
--   Creates, changes, or deletes resource records sets.
--
--   <http://docs.amazonwebservices.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html>
--
module Aws.Route53.Commands.ChangeResourceRecordSets where

import           Aws.Route53.Core
import           Aws.Core
import           Text.Hamlet.XML            (xml)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Text.XML                   as XML
import qualified Data.ByteString            as B

data ACTION = CREATE | DELETE 
            deriving (Show)

-- TODO enforce constrains either via type or dynamically on creation or usage
data ChangeResourceRecordSets = ChangeResourceRecordSets
                      { crrHostedZoneId :: T.Text
                      , crrComment :: Maybe T.Text
                      , crrsChanges :: [(ACTION, ResourceRecordSet)]
                      } deriving (Show)

data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse
  { crrsrChangeInfo :: ChangeInfo
  } deriving (Show)

instance SignQuery ChangeResourceRecordSets where
    type Info ChangeResourceRecordSets = Route53Info
    signQuery ChangeResourceRecordSets{..} = route53SignQuery method resource query body
      where
      method = Post
      resource = "/hostedzone/" `B.append` T.encodeUtf8 crrHostedZoneId `B.append` "/rrset"
      query = []
      body = Just $ XML.Element "{https://route53.amazonaws.com/doc/2012-02-29/}ChangeResourceRecordSetsRequest" []
             [xml|
             <ChangeBatch>
               $maybe c <- crrComment
                 <Comment>#{c}
               <Changes>
                 $forall change <- crrsChanges
                   <Change>
                     <Action>#{T.pack (show (fst change))}
                     ^{[XML.NodeElement (toXml (snd change))]}
             |]

instance ResponseConsumer r ChangeResourceRecordSetsResponse where
    type ResponseMetadata ChangeResourceRecordSetsResponse = Route53Metadata

    responseConsumer _ = route53ResponseConsumer parse
        where 
        parse cursor = do
            route53CheckResponseType () "ChangeResourceRecordSetsResponse" cursor
            changeInfo <- r53Parse cursor
            return $ ChangeResourceRecordSetsResponse changeInfo

instance Transaction ChangeResourceRecordSets ChangeResourceRecordSetsResponse where

