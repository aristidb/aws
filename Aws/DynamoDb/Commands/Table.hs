{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Aws.DynamoDb.Commands.Table(
  -- * Commands
    CreateTable(..)
  , CreateTableResult(..)
  , DescribeTable(..)
  , DescribeTableResult(..)
  , UpdateTable(..)
  , UpdateTableResult(..)
  , DeleteTable(..)
  , DeleteTableResult(..)
  , ListTables(..)
  , ListTablesResult(..)

  -- * Data passed in the commands
  , KeyAttributeType(..)
  , KeyAttributeDefinition(..)
  , KeySchema(..)
  , Projection(..)
  , LocalSecondaryIndex(..)
  , LocalSecondaryIndexStatus(..)
  , ProvisionedThroughput(..)
  , ProvisionedThroughputStatus(..)
  , GlobalSecondaryIndex(..)
  , GlobalSecondaryIndexStatus(..)
  , GlobalSecondaryIndexUpdate(..)
  , TableDescription(..)
) where

import           Aws.Core
import           Aws.DynamoDb.Core
import           Control.Applicative
import           Data.Aeson ((.=), (.:), (.!=), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Char (toUpper)
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Data.HashMap.Strict   as M
import           GHC.Generics (Generic)

capitalizeOpt :: A.Options
capitalizeOpt = A.defaultOptions { A.fieldLabelModifier = \x -> case x of
                                                                  (c:cs) -> toUpper c : cs
                                                                  [] -> []
                                 }


dropOpt :: Int -> A.Options
dropOpt d = A.defaultOptions { A.fieldLabelModifier = drop d }


-- | The type of a key attribute that appears in the table key or as a key in one of the indices.
data KeyAttributeType = AttrStringT | AttrNumberT | AttrBinaryT
    deriving (Show, Eq, Enum, Bounded, Generic)
instance A.ToJSON KeyAttributeType where
    toJSON AttrStringT = A.String "S"
    toJSON AttrNumberT = A.String "N"
    toJSON AttrBinaryT = A.String "B"
instance A.FromJSON KeyAttributeType where
    parseJSON (A.String str) =
        case str of
            "S" -> return AttrStringT
            "N" -> return AttrNumberT
            "B" -> return AttrBinaryT
            _   -> fail $ "Invalid attribute type " ++ T.unpack str
    parseJSON _ = fail "Attribute type must be a string"

-- | A key attribute that appears in the table key or as a key in one of the indices.
data KeyAttributeDefinition
    = KeyAttributeDefinition {
        attributeName :: T.Text
      , attributeType :: KeyAttributeType
      }
    deriving (Show, Generic)
instance A.ToJSON KeyAttributeDefinition where
    toJSON = A.genericToJSON capitalizeOpt
instance A.FromJSON KeyAttributeDefinition where
    parseJSON = A.genericParseJSON capitalizeOpt

-- | The key schema can either be a hash of a single attribute name or a hash attribute name
-- and a range attribute name.
data KeySchema = KeyHashOnly T.Text
               | KeyHashAndRange T.Text T.Text
    deriving (Show)
instance A.ToJSON KeySchema where
    toJSON (KeyHashOnly a) 
        = A.Array $ V.fromList [ A.object [ "AttributeName" .= a
                                          , "KeyType" .= ("HASH" :: T.Text)
                                          ]
                               ]
    toJSON (KeyHashAndRange hash range) 
        = A.Array $ V.fromList [ A.object [ "AttributeName" .= hash
                                          , "KeyType" .= ("HASH" :: T.Text)
                                          ]
                               , A.object [ "AttributeName" .= range
                                          , "KeyType" .= ("RANGE" :: T.Text)
                                          ]
                               ]
instance A.FromJSON KeySchema where
    parseJSON (A.Array v) =
        case V.length v of
            1 -> do obj <- A.parseJSON (v V.! 0)
                    kt <- obj .: "KeyType"
                    if kt /= ("HASH" :: T.Text)
                        then fail "With only one key, the type must be HASH"
                        else KeyHashOnly <$> obj .: "AttributeName"

            2 -> do hash <- A.parseJSON (v V.! 0)
                    range <- A.parseJSON (v V.! 1)
                    hkt <- hash .: "KeyType"
                    rkt <- range .: "KeyType"
                    if hkt /= ("HASH" :: T.Text) || rkt /= ("RANGE" :: T.Text)
                        then fail "With two keys, one must be HASH and the other RANGE"
                        else KeyHashAndRange <$> hash .: "AttributeName"
                                             <*> range .: "AttributeName"
            _ -> fail "Key schema must have one or two entries"
    parseJSON _ = fail "Key schema must be an array"

-- | This determines which attributes are projected into a secondary index.
data Projection = ProjectKeysOnly
                | ProjectAll
                | ProjectInclude [T.Text]
    deriving Show
instance A.ToJSON Projection where
    toJSON ProjectKeysOnly    = A.object [ "ProjectionType" .= ("KEYS_ONLY" :: T.Text) ]
    toJSON ProjectAll         = A.object [ "ProjectionType" .= ("ALL" :: T.Text) ]
    toJSON (ProjectInclude a) = A.object [ "ProjectionType" .= ("INCLUDE" :: T.Text)
                                         , "NonKeyAttributes" .= a
                                         ]
instance A.FromJSON Projection where
    parseJSON (A.Object o) = do
        ty <- (o .: "ProjectionType") :: A.Parser T.Text
        case ty of
            "KEYS_ONLY" -> return ProjectKeysOnly
            "ALL" -> return ProjectAll
            "INCLUDE" -> ProjectInclude <$> o .: "NonKeyAttributes"
            _ -> fail "Invalid projection type"
    parseJSON _ = fail "Projection must be an object"
      
-- | Describes a single local secondary index.  The KeySchema MUST share the same hash key attribute
-- as the parent table, only the range key can differ.
data LocalSecondaryIndex
    = LocalSecondaryIndex {
        localIndexName :: T.Text
      , localKeySchema :: KeySchema
      , localProjection :: Projection
      }
    deriving (Show, Generic)
instance A.ToJSON LocalSecondaryIndex where
    toJSON = A.genericToJSON $ dropOpt 5
instance A.FromJSON LocalSecondaryIndex where
    parseJSON = A.genericParseJSON $ dropOpt 5

-- | This is returned by AWS to describe the local secondary index.
data LocalSecondaryIndexStatus
    = LocalSecondaryIndexStatus {
        locStatusIndexName :: T.Text
      , locStatusIndexSizeBytes :: Integer
      , locStatusItemCount :: Integer
      , locStatusKeySchema :: KeySchema
      , locStatusProjection :: Projection
      }
    deriving (Show, Generic)
instance A.FromJSON LocalSecondaryIndexStatus where
    parseJSON = A.genericParseJSON $ dropOpt 9

-- | The target provisioned throughput you are requesting for the table or global secondary index.
data ProvisionedThroughput
    = ProvisionedThroughput {
        readCapacityUnits :: Int
      , writeCapacityUnits :: Int
      }
    deriving (Show, Generic)
instance A.ToJSON ProvisionedThroughput where
    toJSON = A.genericToJSON capitalizeOpt
instance A.FromJSON ProvisionedThroughput where
    parseJSON = A.genericParseJSON capitalizeOpt

-- | This is returned by AWS as the status of the throughput for a table or global secondary index.
data ProvisionedThroughputStatus
    = ProvisionedThroughputStatus {
        statusLastDecreaseDateTime :: UTCTime
      , statusLastIncreaseDateTime :: UTCTime
      , statusNumberOfDecreasesToday :: Int
      , statusReadCapacityUnits :: Int
      , statusWriteCapacityUnits :: Int
      }
    deriving (Show, Generic)
instance A.FromJSON ProvisionedThroughputStatus where
    parseJSON = A.withObject "Throughput status must be an object" $ \o ->
        ProvisionedThroughputStatus
            <$> (posixSecondsToUTCTime . fromInteger <$> o .:? "LastDecreaseDateTime" .!= 0)
            <*> (posixSecondsToUTCTime . fromInteger <$> o .:? "LastIncreaseDateTime" .!= 0)
            <*> o .:? "NumberOfDecreasesToday" .!= 0
            <*> o .: "ReadCapacityUnits"
            <*> o .: "WriteCapacityUnits"

-- | Describes a global secondary index.
data GlobalSecondaryIndex
    = GlobalSecondaryIndex {
        globalIndexName :: T.Text
      , globalKeySchema :: KeySchema
      , globalProjection :: Projection
      , globalProvisionedThroughput :: ProvisionedThroughput
      }
    deriving (Show, Generic)
instance A.ToJSON GlobalSecondaryIndex where
    toJSON = A.genericToJSON $ dropOpt 6
instance A.FromJSON GlobalSecondaryIndex where
    parseJSON = A.genericParseJSON $ dropOpt 6

-- | This is returned by AWS to describe the status of a global secondary index.
data GlobalSecondaryIndexStatus
    = GlobalSecondaryIndexStatus {
        gStatusIndexName :: T.Text
      , gStatusIndexSizeBytes :: Integer
      , gStatusIndexStatus :: T.Text
      , gStatusItemCount :: Integer
      , gStatusKeySchema :: KeySchema
      , gStatusProjection :: Projection
      , gStatusProvisionedThroughput :: ProvisionedThroughputStatus
      }
    deriving (Show, Generic)
instance A.FromJSON GlobalSecondaryIndexStatus where
    parseJSON = A.genericParseJSON $ dropOpt 7

-- | This is used to request a change in the provisioned throughput of a global secondary index as
-- part of an 'UpdateTable' operation.
data GlobalSecondaryIndexUpdate
    = GlobalSecondaryIndexUpdate {
        gUpdateIndexName :: T.Text
      , gUpdateProvisionedThroughput :: ProvisionedThroughput
      }
    deriving (Show, Generic)
instance A.ToJSON GlobalSecondaryIndexUpdate where
    toJSON gi = A.object ["Update" .= A.genericToJSON (dropOpt 7) gi]

-- | This describes the table and is the return value from AWS for all the table-related commands.
data TableDescription
    = TableDescription {
        rTableName :: T.Text
      , rTableSizeBytes :: Integer
      , rTableStatus :: T.Text -- ^ one of CREATING, UPDATING, DELETING, ACTIVE
      , rCreationDateTime :: UTCTime
      , rItemCount :: Integer
      , rAttributeDefinitions :: [KeyAttributeDefinition]
      , rKeySchema :: KeySchema
      , rProvisionedThroughput :: ProvisionedThroughputStatus
      , rLocalSecondaryIndexes :: [LocalSecondaryIndexStatus]
      , rGlobalSecondaryIndexes :: [GlobalSecondaryIndexStatus]
      }
    deriving (Show, Generic)
instance A.FromJSON TableDescription where
    parseJSON = A.withObject "Table must be an object" $ \o -> do
        t <- case (M.lookup "Table" o, M.lookup "TableDescription" o) of
                (Just (A.Object t), _) -> return t
                (_, Just (A.Object t)) -> return t
                _ -> fail "Table description must have key 'Table' or 'TableDescription'"
        TableDescription <$> t .: "TableName"
                         <*> t .: "TableSizeBytes"
                         <*> t .: "TableStatus"
                         <*> (posixSecondsToUTCTime . fromInteger <$> t .: "CreationDateTime")
                         <*> t .: "ItemCount"
                         <*> t .: "AttributeDefinitions"
                         <*> t .: "KeySchema"
                         <*> t .: "ProvisionedThroughput"
                         <*> t .:? "LocalSecondaryIndexes" .!= []
                         <*> t .:? "GlobalSecondaryIndexes" .!= []
             
{- Can't derive these instances onto the return values
instance ResponseConsumer r TableDescription where
    type ResponseMetadata TableDescription = DyMetadata
    responseConsumer _ _ = ddbResponseConsumer
instance AsMemoryResponse TableDescription where
    type MemoryResponse TableDescription = TableDescription
    loadToMemory = return
-}

-------------------------------------------------------------------------------
--- Commands
-------------------------------------------------------------------------------

data CreateTable
    = CreateTable {
        createTableName :: T.Text
      , createAttributeDefinitions :: [KeyAttributeDefinition] -- ^ only attributes appearing in a key must be listed here
      , createKeySchema :: KeySchema
      , createProvisionedThroughput :: ProvisionedThroughput
      , createLocalSecondaryIndexes :: [LocalSecondaryIndex] -- ^ at most 5 local secondary indices are allowed
      , createGlobalSecondaryIndexes :: [GlobalSecondaryIndex]
      }
    deriving (Show, Generic)
instance A.ToJSON CreateTable where
    toJSON ct = A.object $ m ++ lindex ++ gindex
        where
            m = [ "TableName" .= createTableName ct
                , "AttributeDefinitions" .= createAttributeDefinitions ct
                , "KeySchema" .= createKeySchema ct
                , "ProvisionedThroughput" .= createProvisionedThroughput ct
                ]
            -- AWS will error with 500 if (LocalSecondaryIndexes : []) is present in the JSON
            lindex = if null (createLocalSecondaryIndexes ct)
                        then []
                        else [ "LocalSecondaryIndexes" .= createLocalSecondaryIndexes ct ]
            gindex = if null (createGlobalSecondaryIndexes ct)
                        then []
                        else [ "GlobalSecondaryIndexes" .= createGlobalSecondaryIndexes ct ]
--instance A.ToJSON CreateTable where
--    toJSON = A.genericToJSON $ dropOpt 6
        
-- | ServiceConfiguration: 'DdbConfiguration'
instance SignQuery CreateTable where
    type ServiceConfiguration CreateTable = DdbConfiguration
    signQuery = ddbSignQuery "CreateTable"

newtype CreateTableResult = CreateTableResult { ctStatus :: TableDescription }
    deriving (Show, A.FromJSON)
-- ResponseConsumer and AsMemoryResponse can't be derived
instance ResponseConsumer r CreateTableResult where
    type ResponseMetadata CreateTableResult = DdbResponse
    responseConsumer _ = ddbResponseConsumer
instance AsMemoryResponse CreateTableResult where
    type MemoryResponse CreateTableResult = TableDescription
    loadToMemory = return . ctStatus

instance Transaction CreateTable CreateTableResult

data DescribeTable
    = DescribeTable {
        dTableName :: T.Text 
      }
    deriving (Show, Generic)
instance A.ToJSON DescribeTable where
    toJSON = A.genericToJSON $ dropOpt 1

-- | ServiceConfiguration: 'DdbConfiguration'
instance SignQuery DescribeTable where
    type ServiceConfiguration DescribeTable = DdbConfiguration
    signQuery = ddbSignQuery "DescribeTable"

newtype DescribeTableResult = DescribeTableResult { dtStatus :: TableDescription }
    deriving (Show, A.FromJSON)
-- ResponseConsumer can't be derived
instance ResponseConsumer r DescribeTableResult where
    type ResponseMetadata DescribeTableResult = DdbResponse
    responseConsumer _ = ddbResponseConsumer
instance AsMemoryResponse DescribeTableResult where
    type MemoryResponse DescribeTableResult = TableDescription
    loadToMemory = return . dtStatus

instance Transaction DescribeTable DescribeTableResult

data UpdateTable
    = UpdateTable {
        updateTableName :: T.Text
      , updateProvisionedThroughput :: ProvisionedThroughput
      , updateGlobalSecondaryIndexUpdates :: [GlobalSecondaryIndexUpdate]
      }
    deriving (Show, Generic)
instance A.ToJSON UpdateTable where
    toJSON = A.genericToJSON $ dropOpt 6
    
-- | ServiceConfiguration: 'DdbConfiguration'
instance SignQuery UpdateTable where
    type ServiceConfiguration UpdateTable = DdbConfiguration
    signQuery = ddbSignQuery "UpdateTable"

newtype UpdateTableResult = UpdateTableResult { uStatus :: TableDescription }
    deriving (Show, A.FromJSON)
-- ResponseConsumer can't be derived
instance ResponseConsumer r UpdateTableResult where
    type ResponseMetadata UpdateTableResult = DdbResponse
    responseConsumer _ = ddbResponseConsumer
instance AsMemoryResponse UpdateTableResult where
    type MemoryResponse UpdateTableResult = TableDescription
    loadToMemory = return . uStatus

instance Transaction UpdateTable UpdateTableResult

data DeleteTable
    = DeleteTable {
        deleteTableName :: T.Text
      }
    deriving (Show, Generic)
instance A.ToJSON DeleteTable where
    toJSON = A.genericToJSON $ dropOpt 6

-- | ServiceConfiguration: 'DdbConfiguration'
instance SignQuery DeleteTable where
    type ServiceConfiguration DeleteTable = DdbConfiguration
    signQuery = ddbSignQuery "DeleteTable"

newtype DeleteTableResult = DeleteTableResult { dStatus :: TableDescription }
    deriving (Show, A.FromJSON)
-- ResponseConsumer can't be derived
instance ResponseConsumer r DeleteTableResult where
    type ResponseMetadata DeleteTableResult = DdbResponse
    responseConsumer _ = ddbResponseConsumer
instance AsMemoryResponse DeleteTableResult where
    type MemoryResponse DeleteTableResult = TableDescription
    loadToMemory = return . dStatus

instance Transaction DeleteTable DeleteTableResult

-- | TODO: currently this does not support restarting a cutoff query because of size.
data ListTables = ListTables
    deriving (Show)
instance A.ToJSON ListTables where
    toJSON _ = A.object []
-- | ServiceConfiguration: 'DdbConfiguration'
instance SignQuery ListTables where
    type ServiceConfiguration ListTables = DdbConfiguration
    signQuery = ddbSignQuery "ListTables"

newtype ListTablesResult
    = ListTablesResult {
        tableNames :: [T.Text] 
      }
    deriving (Show, Generic)
instance A.FromJSON ListTablesResult where
    parseJSON = A.genericParseJSON capitalizeOpt
instance ResponseConsumer r ListTablesResult where
    type ResponseMetadata ListTablesResult = DdbResponse
    responseConsumer _ = ddbResponseConsumer
instance AsMemoryResponse ListTablesResult where
    type MemoryResponse ListTablesResult = [T.Text] 
    loadToMemory = return . tableNames

instance Transaction ListTables ListTablesResult
