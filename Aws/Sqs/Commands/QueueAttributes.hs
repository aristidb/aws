{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.QueueAttributes where

import           Aws.Core
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Text.XML.Cursor       (($/), ($//), (&/), (&|))
import qualified Aws.Sqs.Model         as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Text.XML.Cursor       as Cu

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
  parsedName <- M.parseQueueAttribute name
  return (parsedName, value)

instance ResponseConsumer r GetQueueAttributesResponse where
    type ResponseMetadata GetQueueAttributesResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
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

data SetQueueAttributes = SetQueueAttributes{
  sqaAttribute :: M.QueueAttribute,
  sqaValue :: T.Text,
  sqaQueueName :: M.QueueName 
}deriving (Show)

data SetQueueAttributesResponse = SetQueueAttributesResponse{
} deriving (Show)

instance ResponseConsumer r SetQueueAttributesResponse where
    type ResponseMetadata SetQueueAttributesResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where 
        parse _ = do
          return SetQueueAttributesResponse {}
          
instance SignQuery SetQueueAttributes  where 
    type Info SetQueueAttributes  = SqsInfo
    signQuery SetQueueAttributes {..} = sqsSignQuery SqsQuery { 
                                             sqsQueueName = Just sqaQueueName,
                                             sqsQuery = [("Action", Just "SetQueueAttributes"), 
                                                        ("Attribute.Name", Just $ TE.encodeUtf8 $ M.printQueueAttribute sqaAttribute),
                                                        ("Attribute.Value", Just $ TE.encodeUtf8 sqaValue)]} 

instance Transaction SetQueueAttributes SetQueueAttributesResponse
