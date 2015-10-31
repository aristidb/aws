-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: DynamoDb.Utils
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: BSD3
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- Tests for Haskell SQS bindings
--

module DynamoDb.Utils
(
-- * Static Parameters
  testProtocol
, testRegion
, defaultTableName

-- * Static Configuration
, dyConfiguration

-- * DynamoDb Utils
, simpleDy
, simpleDyT
, dyT
, withTable
, withTable_
, createTestTable
) where

import Aws
import Aws.Core
import qualified Aws.DynamoDb as DY

import Control.Error
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.HTTP.Client as HTTP

import Test.Tasty
import Test.QuickCheck.Instances ()

import System.IO

import Utils

-- -------------------------------------------------------------------------- --
-- Static Test parameters
--
-- TODO make these configurable

testProtocol :: Protocol
testProtocol = HTTP

testRegion :: DY.Region
testRegion = DY.ddbUsWest2

defaultTableName :: T.Text
defaultTableName = "test-table"

-- -------------------------------------------------------------------------- --
-- Dynamo Utils

dyConfiguration :: DY.DdbConfiguration qt
dyConfiguration = DY.DdbConfiguration
    { DY.ddbcRegion = testRegion
    , DY.ddbcProtocol = testProtocol
    , DY.ddbcPort = Nothing
    }

simpleDy
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ DY.DdbConfiguration, MonadIO m)
    => r
    -> m (MemoryResponse a)
simpleDy command = do
    c <- dbgConfiguration
    simpleAws c dyConfiguration command

simpleDyT
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ DY.DdbConfiguration, MonadBaseControl IO m, MonadIO m)
    => r
    -> ExceptT T.Text m (MemoryResponse a)
simpleDyT = tryT . simpleDy

dyT
    :: (Transaction r a, ServiceConfiguration r ~ DY.DdbConfiguration)
    => Configuration
    -> HTTP.Manager
    -> r
    -> ExceptT T.Text IO a
dyT cfg manager req = do
    Response _ r <- liftIO . runResourceT $ aws cfg dyConfiguration manager req
    hoistEither $ fmapL sshow r

withTable
    :: T.Text -- ^ table Name
    -> Int -- ^ read capacity (#(non-consistent) reads * itemsize/4KB)
    -> Int -- ^ write capacity (#writes * itemsize/1KB)
    -> (T.Text -> IO a) -- ^ test tree
    -> IO a
withTable = withTable_ True

withTable_
    :: Bool -- ^ whether to prefix te table name
    -> T.Text -- ^ table Name
    -> Int -- ^ read capacity (#(non-consistent) reads * itemsize/4KB)
    -> Int -- ^ write capacity (#writes * itemsize/1KB)
    -> (T.Text -> IO a) -- ^ test tree
    -> IO a
withTable_ prefix tableName readCapacity writeCapacity f =
    do
      tTableName <- if prefix then testData tableName else return tableName

      let deleteTable = do
            r <- runExceptT . retryT 6 $
                void (simpleDyT $ DY.DeleteTable tTableName) `catchE` \e ->
                    liftIO . T.hPutStrLn stderr $ "attempt to delete table failed: " <> e
            either (error . T.unpack) (const $ return ()) r

      let createTable = do
            r <- runExceptT $ do
                retryT 3 $ tryT $ createTestTable tTableName readCapacity writeCapacity
                retryT 6 $ do
                    tableDesc <- simpleDyT $ DY.DescribeTable tTableName
                    when (DY.rTableStatus tableDesc == "CREATING") $ throwE "Table not ready: status CREATING"
            either (error . T.unpack) return r

      bracket_ createTable deleteTable $ f tTableName

createTestTable
    :: T.Text -- ^ table Name
    -> Int -- ^ read capacity (#(non-consistent) reads * itemsize/4KB)
    -> Int -- ^ write capacity (#writes * itemsize/1KB)
    -> IO ()
createTestTable tableName readCapacity writeCapacity = void . simpleDy $
    DY.createTable
        tableName
        attrs
        (DY.HashOnly keyName)
        throughPut
  where
    keyName = "Id"
    keyType = DY.AttrString
    attrs = [DY.AttributeDefinition keyName keyType]
    throughPut = DY.ProvisionedThroughput
        { DY.readCapacityUnits = readCapacity
        , DY.writeCapacityUnits = writeCapacity
        }


