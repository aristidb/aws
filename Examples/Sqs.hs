{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.Core
import qualified Aws.Sqs as Sqs
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
  let sqscfg = Sqs.sqs Aws.Core.HTTPS Sqs.sqsEndpointUsWest2 False :: Sqs.SqsConfiguration Aws.NormalQuery

  {- List any Queues you have already created in your SQS account -}
  Sqs.ListQueuesResponse qUrls <- Aws.simpleAws cfg sqscfg $ Sqs.ListQueues Nothing
  let origQUrlCount = length qUrls
  putStrLn $ "originally had " ++ (show origQUrlCount) ++ " queue urls"
  mapM_ print qUrls

  {- Create a request object to create a queue and then print out the Queue URL -}
  let qName = "scaledsoftwaretest1"
  let createQReq = Sqs.CreateQueue (Just 8400) qName
  Sqs.CreateQueueResponse qUrl <- Aws.simpleAws cfg sqscfg createQReq
  T.putStrLn $ T.concat ["queue was created with Url: ", qUrl]

  {- Create a QueueName object, sqsQName, to hold the name of this queue for the duration -}
  let awsAccountNum = (T.split (== '/') qUrl) !! 3
  let sqsQName = Sqs.QueueName qName awsAccountNum

  {- list queue attributes -- for this example we will only list the approximateNumberOfMessages in this queue. -}
  let qAttReq = Sqs.GetQueueAttributes sqsQName [Sqs.ApproximateNumberOfMessages]
  Sqs.GetQueueAttributesResponse attPairs <- Aws.simpleAws cfg sqscfg qAttReq
  mapM_ (\(attName, attText) -> T.putStrLn $ T.concat ["     ", Sqs.printQueueAttribute attName, " ", attText]) attPairs

  {- Here we add some messages to the queue -}
  let messages = map (\n -> T.pack $ "msg" ++ (show n)) [1 .. 10]
  {- Add messages to the queue -}
  forM_ messages (\mText -> do
      T.putStrLn $ T.concat ["   Adding: ", mText]
      let sqsSendMessage = Sqs.SendMessage mText sqsQName
      Aws.simpleAws cfg sqscfg sqsSendMessage)

  {- Here we remove messages from the queue one at a time. -}
  let receiveMessageReq = Sqs.ReceiveMessage Nothing [] (Just 1) sqsQName
  let numMessages = length messages
  removedMsgs <- replicateM numMessages $ do
      Sqs.ReceiveMessageResponse msgs <- Aws.simpleAws cfg sqscfg receiveMessageReq
      forM msgs (\msg -> do
                     -- here we remove a message, delete it from the queue, and then return the 
                     -- text sent in the body of the message
                     putStrLn $ "Received " ++ (show $ Sqs.mBody msg)
                     Aws.simpleAws cfg sqscfg $ Sqs.DeleteMessage (Sqs.mReceiptHandle msg) sqsQName
                     return $ Sqs.mBody msg)

  {- Now we'll delete the queue we created at the start of this program -}
  putStrLn $ "Deleting the queue: " ++ (show $ Sqs.qName sqsQName)
  let dQReq = Sqs.DeleteQueue sqsQName
  _ <- Aws.simpleAws cfg sqscfg dQReq

  {- | Let's make sure the queue was actually deleted and that the same number of queues exist at when
     | the program ends as when it started.
  -}
  putStrLn $ "Listing all queueus to check to see if " ++ (show $ Sqs.qName sqsQName) ++ " is gone"
  Sqs.ListQueuesResponse qUrls <- Aws.simpleAws cfg sqscfg $ Sqs.ListQueues Nothing
  mapM_ T.putStrLn qUrls

  if qUrl `elem` qUrls
     then putStrLn $ " *\n *\n * Warning, '" ++ (show qName) ++ "' was not deleted\n"
                  ++ " * This is probably just a race condition."
     else putStrLn $ "     The queue '" ++ (show qName) ++ "' was correctly deleted"

