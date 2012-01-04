{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Aws.SimpleDb.Model
where

import           Aws.SimpleDb.Response
import           Aws.Util
import           Aws.Xml
import           Control.Monad
import           Text.XML.Cursor            (($/), (&|))
import qualified Control.Failure            as F
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Text.XML.Cursor            as Cu

data Attribute a
    = ForAttribute { attributeName :: T.Text, attributeData :: a }
    deriving (Show)

readAttribute :: F.Failure XmlException m => Cu.Cursor -> m (Attribute T.Text)
readAttribute cursor = do
  name <- forceM "Missing Name" $ cursor $/ Cu.laxElement "Name" &| decodeBase64
  value <- forceM "Missing Value" $ cursor $/ Cu.laxElement "Value" &| decodeBase64
  return $ ForAttribute name value

data SetAttribute
    = SetAttribute { setAttribute :: T.Text, isReplaceAttribute :: Bool }
    deriving (Show)

attributeQuery :: (a -> [(B.ByteString, B.ByteString)]) -> Attribute a -> [(B.ByteString, B.ByteString)]
attributeQuery  f (ForAttribute name x) =  ("Name", T.encodeUtf8 name) : f x

addAttribute :: T.Text -> T.Text -> Attribute SetAttribute
addAttribute name value = ForAttribute name (SetAttribute value False)

replaceAttribute :: T.Text -> T.Text -> Attribute SetAttribute
replaceAttribute name value = ForAttribute name (SetAttribute value True)

setAttributeQuery :: SetAttribute -> [(B.ByteString, B.ByteString)]
setAttributeQuery (SetAttribute value replace)
    = ("Value", T.encodeUtf8 value) : [("Replace", awsTrue) | replace]

data DeleteAttribute
    = DeleteAttribute
    | ValuedDeleteAttribute { deleteAttributeValue :: T.Text }
    deriving (Show)

deleteAttributeQuery :: DeleteAttribute -> [(B.ByteString, B.ByteString)]
deleteAttributeQuery DeleteAttribute = []
deleteAttributeQuery (ValuedDeleteAttribute value) = [("Value", T.encodeUtf8 value)]

data ExpectedAttribute
    = ExpectedValue { expectedAttributeValue :: T.Text }
    | ExpectedExists { expectedAttributeExists :: Bool }
    deriving (Show)

expectedValue :: T.Text -> T.Text -> Attribute ExpectedAttribute
expectedValue name value = ForAttribute name (ExpectedValue value)

expectedExists :: T.Text -> Bool -> Attribute ExpectedAttribute
expectedExists name exists = ForAttribute name (ExpectedExists exists)

expectedAttributeQuery :: ExpectedAttribute -> [(B.ByteString, B.ByteString)]
expectedAttributeQuery (ExpectedValue value) = [("Value", T.encodeUtf8 value)]
expectedAttributeQuery (ExpectedExists exists) = [("Exists", awsBool exists)]

data Item a
    = Item { itemName :: T.Text, itemData :: a }
    deriving (Show)

readItem :: F.Failure XmlException m => Cu.Cursor -> m (Item [Attribute T.Text])
readItem cursor = do
  name <- force "Missing Name" <=< sequence $ cursor $/ Cu.laxElement "Name" &| decodeBase64
  attributes <- sequence $ cursor $/ Cu.laxElement "Attribute" &| readAttribute
  return $ Item name attributes

itemQuery :: (a -> [(B.ByteString, B.ByteString)]) -> Item a -> [(B.ByteString, B.ByteString)]
itemQuery f (Item name x) = ("ItemName", T.encodeUtf8 name) : f x
