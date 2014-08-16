{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-------------------------------------------------------------------------------
import           Aws
import           Aws.DynamoDb.Commands
import           Aws.DynamoDb.Core
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Data.Conduit
import qualified Data.Conduit.List     as C
import qualified Data.Text             as T
import           Network.HTTP.Conduit  (withManager)
-------------------------------------------------------------------------------

createTableAndWait :: Environment -> IO ()
createTableAndWait env = do
  let req0 = createTable "devel-1"
        [AttributeDefinition "name" AttrString]
        (HashOnly "name")
        (ProvisionedThroughput 1 1)
  resp0 <- simpleAws env req0
  print resp0

  putStrLn "Waiting for table to be created"
  threadDelay (30 * 1000000)

  let req1 = DescribeTable "devel-1"
  resp1 <- simpleAws env req1
  print resp1

main :: IO ()
main = Aws.withDefaultEnvironment $ \env0 -> do
  let env = env0 { environmentDefaultServiceConfiguration = debugServiceConfig }

  createTableAndWait env `catch` (\DdbError{} -> putStrLn "Table already exists")

  putStrLn "Putting an item..."

  let x = item [ attrAs text "name" "josh"
               , attrAs text "class" "not-so-awesome"]

  let req1 = (putItem "devel-1" x ) { piReturn = URAllOld
                                    , piRetCons =  RCTotal
                                    , piRetMet = RICMSize
                                    }


  resp1 <- simpleAws env req1
  print resp1

  putStrLn "Getting the item back..."

  let req2 = getItem "devel-1" (hk "name" "josh")
  resp2 <- simpleAws env req2
  print resp2

  print =<< simpleAws env
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesome")])

  putStrLn "Updating with false conditional."
  (print =<< simpleAws env
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesomer")])
      { uiExpect = Conditions CondAnd [Condition "name" (DEq "john")] })
    `catch` (\ (e :: DdbError) -> putStrLn ("Eating exception: " ++ show e))

  putStrLn "Getting the item back..."
  print =<< simpleAws env req2


  putStrLn "Updating with true conditional"
  print =<< simpleAws env
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesomer")])
      { uiExpect = Conditions CondAnd [Condition "name" (DEq "josh")] }

  putStrLn "Getting the item back..."
  print =<< simpleAws env req2

  putStrLn "Running a Query command..."
  print =<< simpleAws env (query "devel-1" (Slice (Attribute "name" "josh") Nothing))

  putStrLn "Running a Scan command..."
  print =<< simpleAws env (scan "devel-1")

  putStrLn "Filling table with several items..."
  forM_ [0..30] $ \ i -> do
    threadDelay 50000
    simpleAws env $ putItem "devel-1" $
      item [Attribute "name" (toValue $ T.pack ("lots-" ++ show i)), attrAs int "val" i]

  putStrLn "Now paginating in increments of 5..."
  let q0 = (scan "devel-1") { sLimit = Just 5 }

  xs <- withManager $ \mgr -> do
    awsIteratedList env q0 $$ C.consume
  putStrLn ("Pagination returned " ++ show (length xs) ++ " items")
