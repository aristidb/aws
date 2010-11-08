module Aws.SimpleDb.Model
where
  
import Aws.Query

data Attribute a
    = ForAttribute { attributeName :: String, attributeData :: a }
    deriving (Show)
             
data SetAttribute
    = SetAttribute { setAttribute :: String, isReplaceAttribute :: Bool }
    deriving (Show)

attributeQuery :: (a -> [(String, String)]) -> Attribute a -> [(String, String)]
attributeQuery  f (ForAttribute name x) =  ("Name", name) : f x
             
addAttribute :: String -> String -> Attribute SetAttribute
addAttribute name value = ForAttribute name (SetAttribute value False)

replaceAttribute :: String -> String -> Attribute SetAttribute
replaceAttribute name value = ForAttribute name (SetAttribute value True)
             
setAttributeQuery :: SetAttribute -> [(String, String)]
setAttributeQuery (SetAttribute value replace)
    = ("Value", value) : [("Replace", awsTrue) | replace]
             
data ExpectedAttribute
    = ExpectedValue { expectedAttributeValue :: String }
    | ExpectedExists { expectedAttributeExists :: Bool }
    deriving (Show)
             
expectedValue :: String -> String -> Attribute ExpectedAttribute
expectedValue name value = ForAttribute name (ExpectedValue value)

expectedExists :: String -> Bool -> Attribute ExpectedAttribute
expectedExists name exists = ForAttribute name (ExpectedExists exists)
             
expectedAttributeQuery :: ExpectedAttribute -> [(String, String)]
expectedAttributeQuery (ExpectedValue value) = [("Value", value)]
expectedAttributeQuery (ExpectedExists exists) = [("Exists", awsBool exists)]

data Item a
    = Item { itemName :: String, itemData :: a }
    deriving (Show)
             
itemQuery :: (a -> [(String, String)]) -> Item a -> [(String, String)]
itemQuery f (Item name x) = ("ItemName", name) : f x
