module AWS.SimpleDB.Actions
where
  
import AWS.Query  

data CreateDomain
    = CreateDomain {
        cdDomainName :: String
      }
    deriving (Show)
             
data DeleteDomain
    = DeleteDomain {
        ddDomainName :: String
      }
    deriving (Show)
             
data ListDomains
    = ListDomains {
        ldMaxNumberOfDomains :: Int
      , ldNextToken :: String
      }
    deriving (Show)
             
data DomainMetadata
    = DomainMetadata {
        dmDomainName :: String
      }
    deriving (Show)
             
data GetAttributes
    = GetAttributes {
        gaItemName :: String
      , gaAttributeName :: Maybe String
      , gaConsistentRead :: Bool
      , gaDomainName :: String
      }
    deriving (Show)
             
data PutAttributes
    = PutAttributes {
        paItemName :: String
      , paAttributes :: [Attribute SetAttribute]
      , paExpected :: [Attribute ExpectedAttribute]
      , paDomainName :: String
      }
    deriving (Show)
             
data Attribute a
    = ForAttribute { attributeName :: String, attributeData :: a }
    deriving (Show)
             
data SetAttribute
    = SetAttribute { setAttribute :: String, replaceAttribute :: Bool }
    deriving (Show)
             
data ExpectedAttribute
    = ExpectedValue { expectedAttributeValue :: String }
    | ExpectedExists { expectedExists :: Bool }
    deriving (Show)

data BatchPutAttributes
    = BatchPutAttributes {
        bpaItems :: [Item [Attribute SetAttribute]]
      , bpaDomainName :: String
      }
    deriving (Show)

data Item a
    = Item { itemName :: String, itemData :: a }
    deriving (Show)
             
data Select
    = Select {
        sSelectExpression :: String
      , sConsistentRead :: Bool
      , sNextToken :: String
      }
    deriving (Show)
