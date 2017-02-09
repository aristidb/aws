{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Aws.DynamoDb.Commands.Table
    ( -- * Commands
      CreateTable(..)
    , createTable
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
    , AttributeType(..)
    , AttributeDefinition(..)
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

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson            ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson            as A
import qualified Data.Aeson.Types      as A
import           Data.Char             (toUpper)
import qualified Data.HashMap.Strict   as M
import           Data.Scientific       (Scientific)
import qualified Data.Text             as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Typeable
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)
import           Prelude
-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.DynamoDb.Core
-------------------------------------------------------------------------------


capitalizeOpt :: A.Options
capitalizeOpt = A.defaultOptions
    { A.fieldLabelModifier = \x -> case x of
                                     (c:cs) -> toUpper c : cs
                                     [] -> []
    }


dropOpt :: Int -> A.Options
dropOpt d = A.defaultOptions { A.fieldLabelModifier = drop d }


-- | The type of a key attribute that appears in the table key or as a
-- key in one of the indices.
data AttributeType = AttrString | AttrNumber | AttrBinary
    deriving (Show, Read, Ord, Typeable, Eq, Enum, Bounded, Generic)

instance A.ToJSON AttributeType where
    toJSON AttrString = A.String "S"
    toJSON AttrNumber = A.String "N"
    toJSON AttrBinary = A.String "B"

instance A.FromJSON AttributeType where
    parseJSON (A.String str) =
        case str of
            "S" -> return AttrString
            "N" -> return AttrNumber
            "B" -> return AttrBinary
            _   -> fail $ "Invalid attribute type " ++ T.unpack str
    parseJSON _ = fail "Attribute type must be a string"

-- | A key attribute that appears in the table key or as a key in one of the indices.
data AttributeDefinition = AttributeDefinition {
      attributeName :: T.Text
    , attributeType :: AttributeType
    } deriving (Eq,Read,Ord,Show,Typeable,Generic)

instance A.ToJSON AttributeDefinition where
    toJSON = A.genericToJSON capitalizeOpt

instance A.FromJSON AttributeDefinition where
    parseJSON = A.genericParseJSON capitalizeOpt

-- | The key schema can either be a hash of a single attribute name or a hash attribute name
-- and a range attribute name.
data KeySchema = HashOnly T.Text
               | HashAndRange T.Text T.Text
    deriving (Eq,Read,Show,Ord,Typeable,Generic)


instance A.ToJSON KeySchema where
    toJSON (HashOnly a)
        = A.Array $ V.fromList [ A.object [ "AttributeName" .= a
                                          , "KeyType" .= (A.String "HASH")
                                          ]
                               ]

    toJSON (HashAndRange hash range)
        = A.Array $ V.fromList [ A.object [ "AttributeName" .= hash
                                          , "KeyType" .= (A.String "HASH")
                                          ]
                               , A.object [ "AttributeName" .= range
                                          , "KeyType" .= (A.String "RANGE")
                                          ]
                               ]

instance A.FromJSON KeySchema where
    parseJSON (A.Array v) =
        case V.length v of
            1 -> do obj <- A.parseJSON (v V.! 0)
                    kt <- obj .: "KeyType"
                    if kt /= ("HASH" :: T.Text)
                        then fail "With only one key, the type must be HASH"
                        else HashOnly <$> obj .: "AttributeName"

            2 -> do hash <- A.parseJSON (v V.! 0)
                    range <- A.parseJSON (v V.! 1)
                    hkt <- hash .: "KeyType"
                    rkt <- range .: "KeyType"
                    if hkt /= ("HASH" :: T.Text) || rkt /= ("RANGE" :: T.Text)
                        then fail "With two keys, one must be HASH and the other RANGE"
                        else HashAndRange <$> hash .: "AttributeName"
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

-- | Describes a single local secondary index. The KeySchema MUST
-- share the same hash key attribute as the parent table, only the
-- range key can differ.
data LocalSecondaryIndex
    = LocalSecondaryIndex {
        localIndexName  :: T.Text
      , localKeySchema  :: KeySchema
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
        locStatusIndexName      :: T.Text
      , locStatusIndexSizeBytes :: Integer
      , locStatusItemCount      :: Integer
      , locStatusKeySchema      :: KeySchema
      , locStatusProjection     :: Projection
      }
    deriving (Show, Generic)
instance A.FromJSON LocalSecondaryIndexStatus where
    parseJSON = A.genericParseJSON $ dropOpt 9

-- | The target provisioned throughput you are requesting for the table or global secondary index.
data ProvisionedThroughput
    = ProvisionedThroughput {
        readCapacityUnits  :: Int
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
        statusLastDecreaseDateTime   :: UTCTime
      , statusLastIncreaseDateTime   :: UTCTime
      , statusNumberOfDecreasesToday :: Int
      , statusReadCapacityUnits      :: Int
      , statusWriteCapacityUnits     :: Int
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
        globalIndexName             :: T.Text
      , globalKeySchema             :: KeySchema
      , globalProjection            :: Projection
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
        gStatusIndexName             :: T.Text
      , gStatusIndexSizeBytes        :: Integer
      , gStatusIndexStatus           :: T.Text
      , gStatusItemCount             :: Integer
      , gStatusKeySchema             :: KeySchema
      , gStatusProjection            :: Projection
      , gStatusProvisionedThroughput :: ProvisionedThroughputStatus
      }
    deriving (Show, Generic)
instance A.FromJSON GlobalSecondaryIndexStatus where
    parseJSON = A.genericParseJSON $ dropOpt 7

-- | This is used to request a change in the provisioned throughput of
-- a global secondary index as part of an 'UpdateTable' operation.
data GlobalSecondaryIndexUpdate
    = GlobalSecondaryIndexUpdate {
        gUpdateIndexName             :: T.Text
      , gUpdateProvisionedThroughput :: ProvisionedThroughput
      }
    deriving (Show, Generic)
instance A.ToJSON GlobalSecondaryIndexUpdate where
    toJSON gi = A.object ["Update" .= A.genericToJSON (dropOpt 7) gi]

-- | This describes the table and is the return value from AWS for all
-- the table-related commands.
data TableDescription
    = TableDescription {
        rTableName              :: T.Text
      , rTableSizeBytes         :: Integer
      , rTableStatus            :: T.Text -- ^ one of CREATING, UPDATING, DELETING, ACTIVE
      , rCreationDateTime       :: Maybe UTCTime
      , rItemCount              :: Integer
      , rAttributeDefinitions   :: [AttributeDefinition]
      , rKeySchema              :: Maybe KeySchema
      , rProvisionedThroughput  :: ProvisionedThroughputStatus
      , rLocalSecondaryIndexes  :: [LocalSecondaryIndexStatus]
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
                         <*> (fmap (posixSecondsToUTCTime . fromInteger . (round :: Scientific -> Integer)) <$> t .:? "CreationDateTime")
                         <*> t .: "ItemCount"
                         <*> t .:? "AttributeDefinitions" .!= []
                         <*> t .:? "KeySchema"
                         <*> t .: "ProvisionedThroughput"
                         <*> t .:? "LocalSecondaryIndexes" .!= []
                         <*> t .:? "GlobalSecondaryIndexes" .!= []

{- Can't derive these instances onto the return values
instance ResponseConsumer r TableDescription where
    type ResponseMetadata TableDescription = DyMetadata
    responseConsumer _ _ _ = ddbResponseConsumer
instance AsMemoryResponse TableDescription where
    type MemoryResponse TableDescription = TableDescription
    loadToMemory = return
-}

-------------------------------------------------------------------------------
--- Commands
-------------------------------------------------------------------------------

data CreateTable = CreateTable {
      createTableName              :: T.Text
    , createAttributeDefinitions   :: [AttributeDefinition]
    -- ^ only attributes appearing in a key must be listed here
    , createKeySchema              :: KeySchema
    , createProvisionedThroughput  :: ProvisionedThroughput
    , createLocalSecondaryIndexes  :: [LocalSecondaryIndex]
    -- ^ at most 5 local secondary indices are allowed
    , createGlobalSecondaryIndexes :: [GlobalSecondaryIndex]
    } deriving (Show, Generic)

createTable :: T.Text -- ^ Table name
            -> [AttributeDefinition]
            -> KeySchema
            -> ProvisionedThroughput
            -> CreateTable
createTable tn ad ks p = CreateTable tn ad ks p [] []

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
    responseConsumer _ _ = ddbResponseConsumer
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
    responseConsumer _ _ = ddbResponseConsumer
instance AsMemoryResponse DescribeTableResult where
    type MemoryResponse DescribeTableResult = TableDescription
    loadToMemory = return . dtStatus

instance Transaction DescribeTable DescribeTableResult

data UpdateTable
    = UpdateTable {
        updateTableName                   :: T.Text
      , updateProvisionedThroughput       :: ProvisionedThroughput
      , updateGlobalSecondaryIndexUpdates :: [GlobalSecondaryIndexUpdate]
      }
    deriving (Show, Generic)
instance A.ToJSON UpdateTable where
    toJSON a = A.object
        $ "TableName" .= updateTableName a
        : "ProvisionedThroughput" .= updateProvisionedThroughput a
        : case updateGlobalSecondaryIndexUpdates a of
            [] -> []
            l -> [ "GlobalSecondaryIndexUpdates" .= l ]

-- | ServiceConfiguration: 'DdbConfiguration'
instance SignQuery UpdateTable where
    type ServiceConfiguration UpdateTable = DdbConfiguration
    signQuery = ddbSignQuery "UpdateTable"

newtype UpdateTableResult = UpdateTableResult { uStatus :: TableDescription }
    deriving (Show, A.FromJSON)
-- ResponseConsumer can't be derived
instance ResponseConsumer r UpdateTableResult where
    type ResponseMetadata UpdateTableResult = DdbResponse
    responseConsumer _ _ = ddbResponseConsumer
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
    responseConsumer _ _ = ddbResponseConsumer
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
    responseConsumer _ _ = ddbResponseConsumer
instance AsMemoryResponse ListTablesResult where
    type MemoryResponse ListTablesResult = [T.Text]
    loadToMemory = return . tableNames

instance Transaction ListTables ListTablesResult
