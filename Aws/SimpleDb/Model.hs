{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Aws.SimpleDb.Model
where
  
import           Aws.Query
import           Aws.SimpleDb.Error
import           Aws.SimpleDb.Response
import           Aws.Util
import           Control.Monad.Compose.Class
import           Text.XML.Monad
import qualified Data.ByteString             as B
import qualified Data.ByteString.UTF8        as BU
import qualified Text.XML.Light              as XL

data Attribute a
    = ForAttribute { attributeName :: String, attributeData :: a }
    deriving (Show)

readAttribute :: Xml SdbError XL.Element (Attribute String)
readAttribute = do
  name <- decodeBase64 <<< findElementNameUI "Name"
  value <- decodeBase64 <<< findElementNameUI "Value"
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

readItem :: Xml SdbError XL.Element (Item [Attribute String])
readItem = do
  name <- decodeBase64 <<< findElementNameUI "Name"
  attributes <- inList readAttribute <<< findElementsNameUI "Attribute"
  return $ Item name attributes
             
itemQuery :: (a -> [(B.ByteString, B.ByteString)]) -> Item a -> [(B.ByteString, B.ByteString)]
itemQuery f (Item name x) = ("ItemName", BU.fromString name) : f x
