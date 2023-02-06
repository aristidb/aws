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
-- Tests for Haskell SQS bindings
--

module Main
( main
) where

import Aws
import Aws.Core
import qualified Aws.Sqs as SQS

import Control.Arrow (second)
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import Data.IORef
import qualified Data.List as L
import qualified Data.Text as T
import Data.Monoid
import Prelude

import qualified Network.HTTP.Client as HTTP

import Test.Tasty
import Test.QuickCheck.Instances ()

import System.Environment
import System.Exit

import Utils

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
    , "    cabal test --test-option=--run-with-aws-credentials sqs-tests"
    , ""
    ]

tests :: TestTree
tests = withQueueTest defaultQueueName $ \getQueueParams -> testGroup "SQS Tests"
    [ test_queue
    , test_message getQueueParams
    , test_core getQueueParams
    ]

-- -------------------------------------------------------------------------- --
-- Static Test parameters
--
-- TODO make these configurable

testProtocol :: Protocol
testProtocol = HTTP

testSqsEndpoint :: SQS.Endpoint
testSqsEndpoint = SQS.sqsEndpointUsWest2

defaultQueueName :: T.Text
defaultQueueName = "test-queue"

-- -------------------------------------------------------------------------- --
-- SQS Utils

sqsQueueName :: T.Text -> SQS.QueueName
sqsQueueName url = SQS.QueueName (sqsQueueNameText url) (sqsAccountIdText url)

sqsQueueNameText :: T.Text -> T.Text
sqsQueueNameText url = T.split (== '/') url !! 4

sqsAccountIdText :: T.Text -> T.Text
sqsAccountIdText url = T.split (== '/') url !! 3

sqsConfiguration :: SQS.SqsConfiguration qt
sqsConfiguration = SQS.SqsConfiguration
    { SQS.sqsProtocol = testProtocol
    , SQS.sqsEndpoint = testSqsEndpoint
    , SQS.sqsPort = 80
    , SQS.sqsUseUri = False
    , SQS.sqsDefaultExpiry = 180
    }

sqsT
    :: (Transaction r a, ServiceConfiguration r ~ SQS.SqsConfiguration)
    => Configuration
    -> HTTP.Manager
    -> r
    -> ExceptT T.Text IO a
sqsT cfg manager req = do
    Response _ r <- liftIO . runResourceT $ aws cfg sqsConfiguration manager req
    hoistEither $ fmapL sshow r

simpleSqs
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SQS.SqsConfiguration, MonadIO m)
    => r
    -> m (MemoryResponse a)
simpleSqs command = do
    c <- baseConfiguration
    simpleAws c sqsConfiguration command

simpleSqsT
    :: (AsMemoryResponse a, Transaction r a, ServiceConfiguration r ~ SQS.SqsConfiguration, MonadBaseControl IO m, MonadIO m)
    => r
    -> ExceptT T.Text m (MemoryResponse a)
simpleSqsT = tryT . simpleSqs

withQueueTest
    :: T.Text -- ^ Queue name
    -> (IO (T.Text, SQS.QueueName) -> TestTree) -- ^ test tree
    -> TestTree
withQueueTest queueName f = withResource createQueue deleteQueue $ \getQueueUrl ->
    f $ do
        url <- getQueueUrl
        return (url, sqsQueueName url)
  where
    createQueue = do
        SQS.CreateQueueResponse url <- simpleSqs $ SQS.CreateQueue Nothing queueName
        return url
    deleteQueue url = void $ simpleSqs (SQS.DeleteQueue (sqsQueueName url))

-- -------------------------------------------------------------------------- --
-- Queue Tests

test_queue :: TestTree
test_queue = testGroup "Queue Tests"
    [ eitherTOnceTest1 "CreateListDeleteQueue" prop_createListDeleteQueue
    ]

-- |
--
prop_createListDeleteQueue
    :: T.Text -- ^ queue name
    -> ExceptT T.Text IO ()
prop_createListDeleteQueue queueName = do
    tQueueName <- testData queueName
    SQS.CreateQueueResponse queueUrl <- simpleSqsT $ SQS.CreateQueue Nothing tQueueName
    let queue = sqsQueueName queueUrl
    flip catchE (\e -> deleteQueue queue >> throwE e) $ do
        retryT 6 $ do
            SQS.ListQueuesResponse allQueueUrls <- simpleSqsT (SQS.ListQueues Nothing)
            unless (queueUrl `elem` allQueueUrls)
                . throwE $ "queue " <> sshow queueUrl <> " not listed"
        deleteQueue queue
  where
    deleteQueue queueUrl = void $ simpleSqsT (SQS.DeleteQueue queueUrl)

-- -------------------------------------------------------------------------- --
-- Message Tests

test_message :: IO (T.Text, SQS.QueueName) -> TestTree
test_message getQueueParams = testGroup "Queue Tests"
    [ eitherTOnceTest0 "SendReceiveDeleteMessage" $ do
        (_, queue) <- liftIO getQueueParams
        prop_sendReceiveDeleteMessage queue
    , eitherTOnceTest0 "SendReceiveDeleteMessageLongPolling" $ do
        (_, queue) <- liftIO getQueueParams
        prop_sendReceiveDeleteMessageLongPolling queue
    , eitherTOnceTest0 "SendReceiveDeleteMessageLongPolling1" $ do
        (_, queue) <- liftIO getQueueParams
        prop_sendReceiveDeleteMessageLongPolling1 queue
    ]

-- | Simple send and short-polling receive. First sends all messages
-- and receives messages thereafter one by one.
--
prop_sendReceiveDeleteMessage
    :: SQS.QueueName
    -> ExceptT T.Text IO ()
prop_sendReceiveDeleteMessage queue = do

    -- a visibility timeout should be used only if either @receiveBatch == 1@
    -- or no retry is used so that all received messages are handled.
    let visTimeout = Just 60
    let delay = Just 0
    let poll = Nothing -- no consistent receive (any number of messages up to the requested number can be returned)
    let receiveBatch = 1
    let msgNum = 10

    let messages = map (\i -> "message" <> sshow i) [1 .. msgNum]

    -- send messages
    forM_ messages $ \msg -> void . simpleSqsT $ SQS.SendMessage msg queue [] delay

    recMsgs <- fmap concat . replicateM msgNum $ do
        msgs <- retryT 5 $ do
            r <- simpleSqsT $ SQS.ReceiveMessage visTimeout [] (Just receiveBatch) [] queue poll
            case r of
                SQS.ReceiveMessageResponse [] -> throwE "no message received"
                SQS.ReceiveMessageResponse t
                    | length t <= receiveBatch -> return t
                    | otherwise -> throwE $ "unexpected number of messages received: " <> sshow (length t)
        forM_ msgs $ \msg -> retryT 5 $
            simpleSqsT $ SQS.DeleteMessage (SQS.mReceiptHandle msg) queue
        return (map SQS.mBody msgs)

    let recv = L.sort recMsgs
    let sent = L.sort messages
    unless (sent == recv)
        $ throwE $ "received messages don't match send messages; sent: "
            <> sshow sent <> "; got: " <> sshow recv

-- | Checks for consistent receive: There is no message delay, so all messages
-- are available when the first receive is requested. By enabling long-polling
-- (with value 0) we force SQS to do a consistent receive.
--
prop_sendReceiveDeleteMessageLongPolling
    :: SQS.QueueName
    -> ExceptT T.Text IO ()
prop_sendReceiveDeleteMessageLongPolling queue = do

    let delay = Nothing
    let visTimeout = Just 60
    let poll = Just 1 -- consistent receive (maximum available number of requested messages is returned)
    let receiveBatch = 10
    let msgNum = 40 -- this must be a multiple of 'receiveBatch'

    let messages = map (\i -> "message" <> sshow i) [1 .. msgNum]

    -- send messages
    forM_ messages $ \msg -> void . simpleSqsT $ SQS.SendMessage msg queue [] delay

    recMsgs <- fmap concat . replicateM (msgNum `div` receiveBatch) $ do
        msgs <- do
            r <- simpleSqsT $ SQS.ReceiveMessage visTimeout [] (Just receiveBatch) [] queue poll
            case r of
                SQS.ReceiveMessageResponse [] -> throwE "no messages received"
                SQS.ReceiveMessageResponse t
                    | length t == receiveBatch -> return t
                    | otherwise -> throwE $ "unexpected number of messages received: " <> sshow (length t)
        forM_ msgs $ \msg -> retryT 5 $
            simpleSqsT $ SQS.DeleteMessage (SQS.mReceiptHandle msg) queue
        return (map SQS.mBody msgs)

    let recv = L.sort recMsgs
    let sent = L.sort messages
    unless (sent == recv)
        $ throwE $ "received messages don't match send messages; sent: "
            <> sshow sent <> "; got: " <> sshow recv

-- | Checks that long polling is actually enabled. We add a delay to the messages
-- and immediately make a receive request with a polling wait time that is larger
-- than the delay. Note that even though polling forces consistent reads, messages
-- will become available with some (small) offset. Therefor we request only a single
-- message at a time.
--
prop_sendReceiveDeleteMessageLongPolling1
    :: SQS.QueueName
    -> ExceptT T.Text IO ()
prop_sendReceiveDeleteMessageLongPolling1 queue = do

    let delay = Just 2
    let visTimeout = Just 60
    let poll = Just 5 -- consistent receive (maximum available number of requested messages is returned)
    let receiveBatch = 1
    let msgNum = 10 -- this must be a multiple of 'receiveBatch'

    let messages = map (\i -> "message" <> sshow i) [1 :: Int .. msgNum]

    recMsgs <- fmap concat . forM messages $ \msg -> do
        void . simpleSqsT $ SQS.SendMessage msg queue [] delay
        msgs <- do
            r <- simpleSqsT $ SQS.ReceiveMessage visTimeout [] (Just receiveBatch) [] queue poll
            case r of
                SQS.ReceiveMessageResponse [] -> throwE "no messages received"
                SQS.ReceiveMessageResponse t
                    | length t == receiveBatch -> return t
                    | otherwise -> throwE $ "unexpected number of messages received: " <> sshow (length t)
        forM_ msgs $ \m -> retryT 5 $
            simpleSqsT $ SQS.DeleteMessage (SQS.mReceiptHandle m) queue
        return (map SQS.mBody msgs)

    let recv = L.sort recMsgs
    let sent = L.sort messages
    unless (sent == recv)
        $ throwE $ "received messages don't match send messages; sent: "
            <> sshow sent <> "; got: " <> sshow recv


-- -------------------------------------------------------------------------- --
-- Test core functionality

test_core :: IO (T.Text, SQS.QueueName) -> TestTree
test_core getQueueParams = testGroup "Core Tests"
    [ eitherTOnceTest0 "connectionReuse" $ do
        (_, queue) <- liftIO getQueueParams
        prop_connectionReuse queue
    ]

prop_connectionReuse
    :: SQS.QueueName
    -> ExceptT T.Text IO ()
prop_connectionReuse queue = do
    c <- liftIO $ do
        cfg <- baseConfiguration

        -- used for counting the number of TCP connections
        ref <- newIORef (0 :: Int)

        -- Use a single manager for all HTTP requests
        manager <- HTTP.newManager (managerSettings ref)
        void $ runExceptT $
            flip catchE (error . T.unpack) . replicateM_ 3 $ do
                void . sqsT cfg manager $ SQS.ListQueues Nothing
                mustFail . sqsT cfg manager $
                    SQS.SendMessage "" (SQS.QueueName "" "") [] Nothing
                void . sqsT cfg manager $
                    SQS.SendMessage "test-message" queue [] Nothing
                void . sqsT cfg manager $
                    SQS.ReceiveMessage Nothing [] Nothing [] queue (Just 20)

        readIORef ref
    unless (c == 1) $
        throwE "The TCP connection has not been reused"
  where

    managerSettings ref = HTTP.defaultManagerSettings
        { HTTP.managerRawConnection = do
            mkConn <- HTTP.managerRawConnection HTTP.defaultManagerSettings
            return $ \a b c -> do
                atomicModifyIORef ref $ \i -> (succ i, ())
                mkConn a b c
        }

