{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Aws.SimpleDb.Model
where
  
import           Aws.SimpleDb.Error
import           Aws.SimpleDb.Response
import           Aws.Util
import           Aws.Xml
import           Control.Monad
import           Text.XML.Enumerator.Cursor (($/), (&|))
import qualified Control.Failure            as F
import qualified Data.ByteString            as B
import qualified Data.ByteString.UTF8       as BU
import qualified Text.XML.Enumerator.Cursor as Cu

data Attribute a
    = ForAttribute { attributeName :: String, attributeData :: a }
    deriving (Show)

readAttribute :: F.Failure XmlException m => Cu.Cursor -> m (Attribute String)
readAttribute cursor = do
  name <- forceM "Missing Name" $ cursor $/ Cu.laxElement "Name" &| decodeBase64
  value <- forceM "Missing Value" $ cursor $/ Cu.laxElement "Value" &| decodeBase64
  return $ ForAttribute name value
             
data SetAttribute
    = SetAttribute { setAttribute :: String, isReplaceAttribute :: Bool }
    deriving (Show)

attributeQuery :: (a -> [(B.ByteString, B.ByteString)]) -> Attribute a -> [(B.ByteString, B.ByteString)]
attributeQuery  f (ForAttribute name x) =  ("Name", BU.fromString name) : f x
             
addAttribute :: String -> String -> Attribute SetAttribute
addAttribute name value = ForAttribute name (SetAttribute value False)

replaceAttribute :: String -> String -> Attribute SetAttribute
replaceAttribute name value = ForAttribute name (SetAttribute value True)
             
setAttributeQuery :: SetAttribute -> [(B.ByteString, B.ByteString)]
setAttributeQuery (SetAttribute value replace)
    = ("Value", BU.fromString value) : [("Replace", awsTrue) | replace]

data DeleteAttribute
    = DeleteAttribute
    | ValuedDeleteAttribute { deleteAttributeValue :: String }
    deriving (Show)

deleteAttributeQuery :: DeleteAttribute -> [(B.ByteString, B.ByteString)]
deleteAttributeQuery DeleteAttribute = []
deleteAttributeQuery (ValuedDeleteAttribute value) = [("Value", BU.fromString value)]
             
data ExpectedAttribute
    = ExpectedValue { expectedAttributeValue :: String }
    | ExpectedExists { expectedAttributeExists :: Bool }
    deriving (Show)
             
expectedValue :: String -> String -> Attribute ExpectedAttribute
expectedValue name value = ForAttribute name (ExpectedValue value)

expectedExists :: String -> Bool -> Attribute ExpectedAttribute
expectedExists name exists = ForAttribute name (ExpectedExists exists)
             
expectedAttributeQuery :: ExpectedAttribute -> [(B.ByteString, B.ByteString)]
expectedAttributeQuery (ExpectedValue value) = [("Value", BU.fromString value)]
expectedAttributeQuery (ExpectedExists exists) = [("Exists", awsBool exists)]

data Item a
    = Item { itemName :: String, itemData :: a }
    deriving (Show)

readItem :: F.Failure XmlException m => Cu.Cursor -> m (Item [Attribute String])
readItem cursor = do
  name <- force "Missing Name" <=< sequence $ cursor $/ Cu.laxElement "Name" &| decodeBase64
  attributes <- sequence $ cursor $/ Cu.laxElement "Attribute" &| readAttribute
  return $ Item name attributes
             
itemQuery :: (a -> [(B.ByteString, B.ByteString)]) -> Item a -> [(B.ByteString, B.ByteString)]
itemQuery f (Item name x) = ("ItemName", BU.fromString name) : f x
