{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module AWS.SimpleDB.Actions
where
  
import           AWS.Query
import qualified Network.HTTP as HTTP
import qualified Data.ByteString.Lazy.Char8 as L

data SDBInfo
    = SDBInfo {
        sdbiProtocol :: Protocol
      , sdbiHttpMethod :: HTTP.RequestMethod
      , sdbiHost :: String
      , sdbiPort :: Int
      }
    deriving (Show)
             
sdbiBaseQuery :: SDBInfo -> Query
sdbiBaseQuery SDBInfo{..} = Query { 
                              api = SimpleDB
                            , method = sdbiHttpMethod
                            , protocol = sdbiProtocol
                            , host = sdbiHost
                            , port = sdbiPort 
                            , path = "/"
                            , query = [("Version", "2009-04-15")]
                            , date = Nothing
                            , metadata = []
                            , body = L.empty
                            }

data CreateDomain
    = CreateDomain {
        cdDomainName :: String
      }
    deriving (Show)
             
instance AsQuery CreateDomain SDBInfo where
    asQuery i CreateDomain{..} = addQuery [("Action", "CreateDomain"), ("DomainName", cdDomainName)] (sdbiBaseQuery i)
             
data DeleteDomain
    = DeleteDomain {
        ddDomainName :: String
      }
    deriving (Show)
             
instance AsQuery DeleteDomain SDBInfo where
    asQuery i DeleteDomain{..} = addQuery [("DomainName", ddDomainName)] (sdbiBaseQuery i)

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
