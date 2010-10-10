{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module AWS.SimpleDB
where
  
import           AWS.Query
import           Data.Maybe
import qualified Network.HTTP               as HTTP
import qualified Data.ByteString.Lazy.Char8 as L

data SDBInfo
    = SDBInfo {
        sdbiProtocol :: Protocol
      , sdbiHttpMethod :: HTTP.RequestMethod
      , sdbiHost :: String
      , sdbiPort :: Int
      }
    deriving (Show)
             
sdbHttpGet :: SDBInfo
sdbHttpGet = SDBInfo HTTP HTTP.GET "sdb.amazonaws.com" (defaultPort HTTP)
                          
sdbHttpPost :: SDBInfo
sdbHttpPost = SDBInfo HTTP HTTP.POST "sdb.amazonaws.com" (defaultPort HTTP)
              
sdbHttpsGet :: SDBInfo
sdbHttpsGet = SDBInfo HTTPS HTTP.GET "sdb.amazonaws.com" (defaultPort HTTPS)
             
sdbHttpsPost :: SDBInfo
sdbHttpsPost = SDBInfo HTTPS HTTP.POST "sdb.amazonaws.com" (defaultPort HTTPS)

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
             
createDomain :: String -> CreateDomain
createDomain name = CreateDomain { cdDomainName = name }
             
instance AsQuery CreateDomain SDBInfo where
    asQuery i CreateDomain{..} = addQuery [("Action", "CreateDomain"), ("DomainName", cdDomainName)] (sdbiBaseQuery i)
             
data DeleteDomain
    = DeleteDomain {
        ddDomainName :: String
      }
    deriving (Show)
             
deleteDomain :: String -> DeleteDomain
deleteDomain name = DeleteDomain { ddDomainName = name }
             
instance AsQuery DeleteDomain SDBInfo where
    asQuery i DeleteDomain{..} = addQuery [("Action", "DeleteDomain"), ("DomainName", ddDomainName)] (sdbiBaseQuery i)

data ListDomains
    = ListDomains {
        ldMaxNumberOfDomains :: Maybe Int
      , ldNextToken :: String
      }
    deriving (Show)

listDomains :: ListDomains
listDomains = ListDomains { ldMaxNumberOfDomains = Nothing, ldNextToken = "" }
             
instance AsQuery ListDomains SDBInfo where
    asQuery i ListDomains{..} = addQuery [("Action", "ListDomains")]
                                . addQueryMaybe show ("MaxNumberOfDomains", ldMaxNumberOfDomains)
                                . addQueryUnless (null ldNextToken) [("NextToken", ldNextToken)]
                                $ sdbiBaseQuery i
                                
data DomainMetadata
    = DomainMetadata {
        dmDomainName :: String
      }
    deriving (Show)
             
domainMetadata :: String -> DomainMetadata
domainMetadata name = DomainMetadata { dmDomainName = name }

instance AsQuery DomainMetadata SDBInfo where
    asQuery i DomainMetadata{..} = addQuery [("Action", "DomainMetadata"), ("DomainName", dmDomainName)] (sdbiBaseQuery i)
             
data GetAttributes
    = GetAttributes {
        gaItemName :: String
      , gaAttributeName :: Maybe String
      , gaConsistentRead :: Bool
      , gaDomainName :: String
      }
    deriving (Show)
             
getAttributes :: String -> String -> GetAttributes
getAttributes item domain = GetAttributes { gaItemName = item, gaAttributeName = Nothing, gaConsistentRead = False, gaDomainName = domain }

instance AsQuery GetAttributes SDBInfo where
    asQuery i GetAttributes{..}
        = addQuery [("Action", "GetAttributes"), ("ItemName", gaItemName), ("DomainName", gaDomainName)]
          . addQueryMaybe id ("AttributeName", gaAttributeName)
          . addQueryIf gaConsistentRead [("ConsistentRead", awsTrue)]
          $ sdbiBaseQuery i
             
data PutAttributes
    = PutAttributes {
        paItemName :: String
      , paAttributes :: [Attribute SetAttribute]
      , paExpected :: [Attribute ExpectedAttribute]
      , paDomainName :: String
      }
    deriving (Show)
             
putAttributes :: String -> [Attribute SetAttribute] -> String -> PutAttributes
putAttributes item attributes domain = PutAttributes { 
                                         paItemName = item
                                       , paAttributes = attributes
                                       , paExpected = []
                                       , paDomainName = domain 
                                       }
                                       
instance AsQuery PutAttributes SDBInfo where
    asQuery i PutAttributes{..}
        = addQuery [("Action", "PutAttributes"), ("ItemName", paItemName), ("DomainName", paDomainName)]
          . addQueryList (attributeQuery setAttributeQuery) "Attribute" paAttributes
          . addQueryList (attributeQuery expectedAttributeQuery) "Expected" paExpected
          $ sdbiBaseQuery i
             
data Attribute a
    = ForAttribute { attributeName :: String, attributeData :: a }
    deriving (Show)
             
attributeQuery :: (a -> [(String, String)]) -> Attribute a -> [(String, String)]
attributeQuery  f (ForAttribute name x) =  ("Name", name) : f x
             
data SetAttribute
    = SetAttribute { setAttribute :: String, isReplaceAttribute :: Bool }
    deriving (Show)
             
addAttribute :: String -> String -> Attribute SetAttribute
addAttribute name value = ForAttribute name (SetAttribute value False)

replaceAttribute :: String -> String -> Attribute SetAttribute
replaceAttribute name value = ForAttribute name (SetAttribute value True)
             
setAttributeQuery :: SetAttribute -> [(String, String)]
setAttributeQuery (SetAttribute value replace)
    = ("Value", value) : (if replace then [("Replace", awsTrue)] else [])
             
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

data BatchPutAttributes
    = BatchPutAttributes {
        bpaItems :: [Item [Attribute SetAttribute]]
      , bpaDomainName :: String
      }
    deriving (Show)
             
batchPutAttributes :: [Item [Attribute SetAttribute]] -> String -> BatchPutAttributes
batchPutAttributes items domain = BatchPutAttributes { bpaItems = items, bpaDomainName = domain }

instance AsQuery BatchPutAttributes SDBInfo where
    asQuery i BatchPutAttributes{..}
        = addQuery [("Action", "BatchPutAttributes"), ("DomainName", bpaDomainName)]
          . addQueryList (itemQuery $ queryList (attributeQuery setAttributeQuery) "Attribute") "Item" bpaItems
          $ sdbiBaseQuery i

data Item a
    = Item { itemName :: String, itemData :: a }
    deriving (Show)
             
itemQuery :: (a -> [(String, String)]) -> Item a -> [(String, String)]
itemQuery f (Item name x) = ("ItemName", name) : f x
             
data Select
    = Select {
        sSelectExpression :: String
      , sConsistentRead :: Bool
      , sNextToken :: String
      }
    deriving (Show)

select :: String -> Select
select expr = Select { sSelectExpression = expr, sConsistentRead = False, sNextToken = "" }

instance AsQuery Select SDBInfo where
    asQuery i Select{..}
        = addQuery [("Action", "Select"), ("SelectExpression", sSelectExpression)]
          . addQueryIf sConsistentRead [("ConsistentRead", awsTrue)]
          . addQueryUnless (null sNextToken) [("NextToken", sNextToken)]
          $ sdbiBaseQuery i
