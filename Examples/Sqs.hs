{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.Core
import qualified Aws.Sqs as Sqs
import Control.Concurrent
import Control.Error
import Control.Monad.IO.Class
import Data.Monoid
import Data.String
import qualified Data.Text.IO as T
import qualified Data.Text    as T
import qualified Data.Text.Read as TR
import Control.Monad (forM_, forM, replicateM)

{-| Created by Tim Perry on September 18, 2013
  |
  | All code relies on a correctly configured ~/.aws-keys and will access that account which
  | may incur charges for the user!
  |
  | This code will demonstrate:
  |       - Listing all queue's attached to the current AWS account.
  |       - Creating a queue
  |       - Adding messages to the queue
  |       - Retrieving messages from the queue
  |       - Deleting messages from the queue
  |          and finally
  |       - Deleting the queue.
  | -}
main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration
  let sqscfg = Sqs.sqs Aws.Core.HTTP Sqs.sqsEndpointUsWest2 False :: Sqs.SqsConfiguration Aws.NormalQuery

  {- List any Queues you have already created in your SQS account -}
  Sqs.ListQueuesResponse qUrls <- Aws.simpleAws cfg sqscfg $ Sqs.ListQueues Nothing
  let origQUrlCount = length qUrls
  putStrLn $ "originally had " ++ show origQUrlCount ++ " queue urls"
  mapM_ print qUrls

  {- Create a request object to create a queue and then print out the Queue URL -}
  let qName = "scaledsoftwaretest1"
  let createQReq = Sqs.CreateQueue (Just 8400) qName
  Sqs.CreateQueueResponse qUrl <- Aws.simpleAws cfg sqscfg createQReq
  T.putStrLn $ T.concat ["queue was created with Url: ", qUrl]

  {- Create a QueueName object, sqsQName, to hold the name of this queue for the duration -}
  let awsAccountNum = T.split (== '/') qUrl !! 3
  let sqsQName = Sqs.QueueName qName awsAccountNum

  {- list queue attributes -- for this example we will only list the approximateNumberOfMessages in this queue. -}
  let qAttReq = Sqs.GetQueueAttributes sqsQName [Sqs.ApproximateNumberOfMessages]
  Sqs.GetQueueAttributesResponse attPairs <- Aws.simpleAws cfg sqscfg qAttReq
  mapM_ (\(attName, attText) -> T.putStrLn $ T.concat ["     ", Sqs.printQueueAttribute attName, " ", attText]) attPairs

  {- Here we add some messages to the queue -}
  let messages = map (\n -> T.pack $ "msg" ++ show n) [1 .. 10]
  {- Add messages to the queue -}
  forM_ messages $ \mText -> do
      T.putStrLn $ "   Adding: " <> mText
      let sqsSendMessage = Sqs.SendMessage mText sqsQName [] (Just 0)
      Sqs.SendMessageResponse _ mid _ <- Aws.simpleAws cfg sqscfg sqsSendMessage
      T.putStrLn $ "      message id: " <> sshow mid

  {- Here we remove messages from the queue one at a time. -}
  let receiveMessageReq = Sqs.ReceiveMessage Nothing [] (Just 1) [] sqsQName (Just 20)
  let numMessages = length messages
  removedMsgs <- replicateM numMessages $ do
      msgs <- eitherT (const $ return []) return . retryT 2 $ do
        Sqs.ReceiveMessageResponse r <- liftIO $ Aws.simpleAws cfg sqscfg receiveMessageReq
        case r of
          [] -> left "no message received"
          _ -> right r
      putStrLn $ "number of messages received: " ++ show (length msgs)
      forM msgs (\msg -> do
                     -- here we remove a message, delete it from the queue, and then return the
                     -- text sent in the body of the message
                     putStrLn $ "   Received " ++ show (Sqs.mBody msg)
                     Aws.simpleAws cfg sqscfg $ Sqs.DeleteMessage (Sqs.mReceiptHandle msg) sqsQName
                     return $ Sqs.mBody msg)

  {- Now we'll delete the queue we created at the start of this program -}
  putStrLn $ "Deleting the queue: " ++ show (Sqs.qName sqsQName)
  let dQReq = Sqs.DeleteQueue sqsQName
  _ <- Aws.simpleAws cfg sqscfg dQReq

  {- | Let's make sure the queue was actually deleted and that the same number of queues exist at when
     | the program ends as when it started.
  -}
  eitherT T.putStrLn T.putStrLn . retryT 4 $ do
    qUrls <- liftIO $ do
      putStrLn $ "Listing all queueus to check to see if " ++ show (Sqs.qName sqsQName) ++ " is gone"
      Sqs.ListQueuesResponse qUrls_ <- Aws.simpleAws cfg sqscfg $ Sqs.ListQueues Nothing
      mapM_ T.putStrLn qUrls_
      return qUrls_

    if qUrl `elem` qUrls
        then left $ " *\n *\n * Warning, '" <> sshow qName <> "' was not deleted\n"
                    <> " * This is probably just a race condition."
        else right $ "     The queue '" <> sshow qName <> "' was correctly deleted"

retryT :: MonadIO m => Int -> EitherT T.Text m a -> EitherT T.Text m a
retryT i f = go 1
  where
    go x
        | x >= i = fmapLT (\e -> "error after " <> sshow x <> " retries: " <> e) f
        | otherwise = f `catchT` \_ -> do
            liftIO $ threadDelay (1000000 * min 60 (2^(x-1)))
            go (succ x)

sshow :: (Show a, IsString b) => a -> b
sshow = fromString . show

