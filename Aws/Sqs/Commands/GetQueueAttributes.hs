{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.GetQueueAttributes where

import           Aws.Response
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Text.XML.Enumerator.Cursor   (($/), ($//), (&/), (&|))
import qualified Data.Text                    as T
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Data.ByteString.Char8 as B

data GetQueueAttributes = GetQueueAttributes {
  gqaQueueName :: M.QueueName,
  gqaAttributes :: [M.QueueAttribute]
}deriving (Show)

data GetQueueAttributesResponse = GetQueueAttributesResponse{
  gqarAttributes :: [(M.QueueAttribute,T.Text)]
} deriving (Show)

parseAttributes :: Cu.Cursor -> [(M.QueueAttribute, T.Text)]
parseAttributes el = do
  name <- force "Missing Name" $ el $/ Cu.laxElement "Name" &/ Cu.content
  value <- force "Missing Value" $ el $/ Cu.laxElement "Value" &/ Cu.content
  return (M.parseQueueAttribute name, value)

instance ResponseIteratee r GetQueueAttributesResponse where
    type ResponseMetadata GetQueueAttributesResponse = SqsMetadata
    responseIteratee _ = sqsXmlResponseIteratee parse
      where
        parse el = do
          let attributes = concat $ el $// Cu.laxElement "Attribute" &| parseAttributes  
          return GetQueueAttributesResponse{ gqarAttributes = attributes }
               
formatAttributes :: [M.QueueAttribute] -> [(B.ByteString, Maybe B.ByteString)]
formatAttributes attrs =
  case length attrs of
    0 -> undefined
    1 -> [("AttributeName", Just $ B.pack $ T.unpack $ M.printQueueAttribute $ attrs !! 0)]
    _ -> zipWith (\ x y -> ((B.concat ["AttributeName.", B.pack $ show $ y]), Just $ B.pack $ T.unpack $ M.printQueueAttribute x) ) attrs [1 :: Integer ..]
          
instance SignQuery GetQueueAttributes where 
    type Info GetQueueAttributes = SqsInfo
    signQuery GetQueueAttributes{..} = sqsSignQuery SqsQuery { 
                                              sqsQueueName = Just gqaQueueName, 
                                              sqsQuery = [("Action", Just "GetQueueAttributes")] ++ (formatAttributes gqaAttributes)}

instance Transaction GetQueueAttributes GetQueueAttributesResponse
