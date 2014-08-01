{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Aws.DynamoDb.Commands.Query
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- Stability   :  experimental
--
-- Implementation of Amazon DynamoDb Query command.
--
-- See: @http:\/\/docs.aws.amazon.com\/amazondynamodb\/latest\/APIReference\/API_Query.html@
----------------------------------------------------------------------------

module Aws.DynamoDb.Commands.Query
    ( Query (..)
    , Slice (..)
    , query
    , QueryResponse (..)
    , queryPageSource
    , queryItemSource
    , paginateQuery
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Conduit        (Producer, (=$=))
import qualified Data.Conduit.List   as C
import           Data.Default
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Typeable
import qualified Data.Vector         as V
-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.DynamoDb.Core
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Low level pagination for 'Query'
paginateQuery
    :: Monad m
    => (Query -> m QueryResponse)
    -- ^ A way to run queries.
    -> Query
    -- ^ A starting point for the pagination
    -> m (Page m Query QueryResponse)
paginateQuery run q0 = do
    res <- run q0
    let next = case qrLastKey res of
          Nothing -> Nothing
          Just _ -> Just $ paginateQuery run q0 { qStartKey = qrLastKey res }
    return $ Page res next


-------------------------------------------------------------------------------
-- | Conduit 'Producer' of 'QueryResponse' pages.
queryPageSource
    :: Monad m
    => (Query -> m QueryResponse)
    -- ^ A way to run 'Query' commands
    -> Query
    -- ^ An initial starting point
    -> Producer m QueryResponse
queryPageSource run q0 = pageSource $ paginateQuery run q0


-------------------------------------------------------------------------------
-- | Stream 'Item's via conduit's 'Producer'.
queryItemSource
    :: Monad m
    => (Query -> m QueryResponse)
    -> Query
    -> Producer m Item
queryItemSource run q0 = queryPageSource run q0 =$= C.concatMap (V.toList . qrItems)


-------------------------------------------------------------------------------
-- | 'Slice' is the primary constraint in a 'Query' command, per AWS
-- requirements.
--
-- All 'Query' commands must specify a hash attribute via 'DEq' and
-- optionally provide a secondary range attribute.
data Slice = Slice {
      sliceHash :: Attribute
    -- ^ Hash value of the primary key or index being used
    , sliceCond :: Maybe Condition
    -- ^ An optional condition specified on the range component, if
    -- present, of the primary key or index being used.
    }  deriving (Eq,Show,Read,Ord,Typeable)



-- | A Query command that uses primary keys for an expedient scan.
data Query = Query {
      qTableName     :: T.Text
    -- ^ Required.
    , qKeyConditions :: Slice
    -- ^ Required. Hash or hash-range main condition.
    , qFilter        :: Conditions
    -- ^ Whether to filter results before returning to client
    , qStartKey      :: Maybe [Attribute]
    -- ^ Exclusive start key to resume a previous query.
    , qLimit         :: Maybe Int
    -- ^ Whether to limit result set size
    , qForwardScan   :: Bool
    -- ^ Set to False for descending results
    , qSelect        :: QuerySelect
    -- ^ What to return from 'Query'
    , qRetCons       :: ReturnConsumption
    , qIndex         :: Maybe T.Text
    -- ^ Whether to use a secondary/global index
    , qConsistent    :: Bool
    } deriving (Eq,Show,Read,Ord,Typeable)


-------------------------------------------------------------------------------
instance ToJSON Query where
    toJSON Query{..} = object $
      catMaybes
        [ (("ExclusiveStartKey" .= ) . attributesJson) <$> qStartKey
        , ("Limit" .= ) <$> qLimit
        , ("IndexName" .= ) <$> qIndex
        ] ++
      conditionsJson "QueryFilter" qFilter ++
      querySelectJson qSelect ++
      [ "ScanIndexForward" .= qForwardScan
      , "TableName".= qTableName
      , "KeyConditions" .= sliceJson qKeyConditions
      , "ReturnConsumedCapacity" .= qRetCons
      , "ConsistentRead" .= qConsistent
      ]


-------------------------------------------------------------------------------
-- | Construct a minimal 'Query' request.
query
    :: T.Text
    -- ^ Table name
    -> Slice
    -- ^ Primary key slice for query
    -> Query
query tn sl = Query tn sl def Nothing Nothing True def def Nothing False


-- | Response to a 'Query' query.
data QueryResponse = QueryResponse {
      qrItems    :: V.Vector Item
    , qrLastKey  :: Maybe [Attribute]
    , qrCount    :: Int
    , qrScanned  :: Int
    , qrConsumed :: Maybe ConsumedCapacity
    } deriving (Eq,Show,Read,Ord)


instance FromJSON QueryResponse where
    parseJSON (Object v) = QueryResponse
        <$> v .:?  "Items" .!= V.empty
        <*> ((do o <- v .: "LastEvaluatedKey"
                 Just <$> parseAttributeJson o)
             <|> pure Nothing)
        <*> v .:  "Count"
        <*> v .:  "ScannedCount"
        <*> v .:? "ConsumedCapacity"
    parseJSON _ = fail "QueryResponse must be an object."


instance Transaction Query QueryResponse


instance SignQuery Query where
    type ServiceConfiguration Query = DdbConfiguration
    signQuery gi = ddbSignQuery "Query" gi


instance ResponseConsumer r QueryResponse where
    type ResponseMetadata QueryResponse = DdbResponse
    responseConsumer _ ref resp = ddbResponseConsumer ref resp


instance AsMemoryResponse QueryResponse where
    type MemoryResponse QueryResponse = QueryResponse
    loadToMemory = return


sliceJson :: Slice -> Value
sliceJson Slice{..} = object (map conditionJson cs)
    where
      cs = maybe [] return sliceCond ++ [hashCond]
      hashCond = Condition (attrName sliceHash) (DEq (attrVal sliceHash))
