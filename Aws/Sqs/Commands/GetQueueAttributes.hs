{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.GetQueueAttributes where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Control.Arrow         (second)
import           Control.Monad
import           Data.Enumerator              ((=$))
import           Data.Maybe
import           Data.Time.Format
import           System.Locale
import           Text.XML.Enumerator.Cursor   (($/), ($//), (&/), (&|), ($|))
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML
import qualified Network.HTTP.Types    as HTTP
import qualified Data.ByteString.UTF8  as BU
import qualified Data.ByteString.Char8 as B
import Debug.Trace

data GetQueueAttributes = GetQueueAttributes {
  gqaQueueName :: String,
  gqaAttributes :: [M.QueueAttribute]
}deriving (Show)

data GetQueueAttributesResponse = GetQueueAttributesResponse{
  gqarAttributes :: [(M.QueueAttribute,T.Text)]
} deriving (Show)

parseAttributes :: Cu.Cursor -> (M.QueueAttribute, T.Text)
parseAttributes el =
  (M.parseAttribute $ head $ el $/ Cu.laxElement "Name" &/ Cu.content, 
   head $ el $/ Cu.laxElement "Value" &/ Cu.content)

gqarParse :: Cu.Cursor -> GetQueueAttributesResponse
gqarParse el = do
  let attributes = Cu.laxElement "GetQueueAttributesResponse" &/ Cu.laxElement "GetQueueAttributesResult" &/ Cu.laxElement "Attribute" &| parseAttributes $ el
  GetQueueAttributesResponse { gqarAttributes = attributes }

instance SqsResponseIteratee GetQueueAttributesResponse where
    sqsResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                            let cursor = Cu.fromDocument doc
                                            return $ gqarParse cursor                                  

formatAttributes :: [M.QueueAttribute] -> [HTTP.QueryItem]
formatAttributes attrs =
  case length attrs of
    0 -> undefined
    1 -> [("AttributeName", Just $ B.pack $ show $ attrs !! 0)]
    _ -> zipWith (\ x y -> ((B.concat ["AttributeName.", B.pack $ show $ y]), Just $ B.pack $ M.printQueueAttribute x) ) attrs [1..]
          
instance SignQuery GetQueueAttributes where 
    type Info GetQueueAttributes = SqsInfo
    signQuery GetQueueAttributes{..} = sqsSignQuery SqsQuery { 
                                              sqsQueueName = Just gqaQueueName, 
                                              sqsQuery = [("Action", Just "GetQueueAttributes")] ++ (formatAttributes gqaAttributes)}

instance Transaction GetQueueAttributes (SqsResponse GetQueueAttributesResponse)
