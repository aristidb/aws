-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: BSD3
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- Tests for Haskell AWS DynamoDb bindings
--

module Main
( main
) where

import qualified Aws.DynamoDb as DY

import Control.Arrow (second)
import Control.Error
import Control.Monad

import qualified Data.List as L
import qualified Data.Text as T

import Test.Tasty
import Test.QuickCheck.Instances ()

import System.Environment
import System.Exit

import Utils
import DynamoDb.Utils

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = do
    args <- getArgs
    runMain args $ map (second tail . span (/= '=')) args
  where
    runMain :: [String] -> [(String,String)] -> IO ()
    runMain args _argsMap
        | any (`elem` helpArgs) args = defaultMain tests
        | "--run-with-aws-credentials" `elem` args =
            withArgs (tastyArgs args) . defaultMain $ tests
        | otherwise = putStrLn help >> exitFailure

    helpArgs = ["--help", "-h"]
    mainArgs =
        [ "--run-with-aws-credentials"
        ]
    tastyArgs args = flip filter args $ \x -> not
        $ any (`L.isPrefixOf` x) mainArgs


help :: String
help = L.intercalate "\n"
    [ ""
    , "NOTE"
    , ""
    , "This test suite accesses the AWS account that is associated with"
    , "the default credentials from the credential file ~/.aws-keys."
    , ""
    , "By running the tests in this test-suite costs for usage of AWS"
    , "services may incur."
    , ""
    , "In order to actually excute the tests in this test-suite you must"
    , "provide the command line options:"
    , ""
    , "    --run-with-aws-credentials"
    , ""
    , "When running this test-suite through cabal you may use the following"
    , "command:"
    , ""
    , "    cabal test --test-option=--run-with-aws-credentials dynamodb-tests"
    , ""
    ]

tests :: TestTree
tests = testGroup "DynamoDb Tests"
    [ test_table
    -- , test_message
    ]

-- -------------------------------------------------------------------------- --
-- Table Tests

test_table :: TestTree
test_table = testGroup "Table Tests"
    [ eitherTOnceTest1 "CreateDescribeDeleteTable" (prop_createDescribeDeleteTable 10 10)
    ]

-- |
--
prop_createDescribeDeleteTable
    :: Int -- ^ read capacity (#(non-consistent) reads * itemsize/4KB)
    -> Int -- ^ write capacity (#writes * itemsize/1KB)
    -> T.Text -- ^ table name
    -> EitherT T.Text IO ()
prop_createDescribeDeleteTable readCapacity writeCapacity tableName = do
    tryT $ createTestTable tTableName readCapacity writeCapacity
    handleT (\e -> deleteTable >> left e) $ do
        retryT 6 . void . simpleDyT $ DY.DescribeTable tTableName
        deleteTable
  where
    tTableName = testData tableName
    deleteTable = retryT 6 . void $ simpleDyT (DY.DeleteTable tTableName)

