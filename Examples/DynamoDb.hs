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
-------------------------------------------------------------------------------

createTableAndWait :: IO ()
createTableAndWait = do
  let req0 = createTable "devel-1"
        [AttributeDefinition "name" AttrString]
        (HashOnly "name")
        (ProvisionedThroughput 1 1)
  resp0 <- runCommand req0
  print resp0

  print "Waiting for table to be created"
  threadDelay (30 * 1000000)

  let req1 = DescribeTable "devel-1"
  resp1 <- runCommand req1
  print resp1

main :: IO ()
main = do
  cfg <- Aws.baseConfiguration

  createTableAndWait `catch` (\DdbError{} -> putStrLn "Table already exists")

  putStrLn "Putting an item..."

  let x = item [ attrAs text "name" "josh"
               , attrAs text "class" "not-so-awesome"]

  let req1 = (putItem "devel-1" x ) { piReturn = URAllOld
                                    , piRetCons =  RCTotal
                                    , piRetMet = RICMSize
                                    }


  resp1 <- runCommand req1
  print resp1

  putStrLn "Getting the item back..."

  let req2 = getItem "devel-1" (hk "name" "josh")
  resp2 <- runCommand req2
  print resp2

  print =<< runCommand
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesome")])

  echo "Updating with false conditional."
  (print =<< runCommand
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesomer")])
      { uiExpect = Conditions CondAnd [Condition "name" (DEq "john")] })
    `catch` (\ (e :: DdbError) -> echo ("Eating exception: " ++ show e))

  echo "Getting the item back..."
  print =<< runCommand req2


  echo "Updating with true conditional"
  print =<< runCommand
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesomer")])
      { uiExpect = Conditions CondAnd [Condition "name" (DEq "josh")] }

  echo "Getting the item back..."
  print =<< runCommand req2

  echo "Running a Query command..."
  print =<< runCommand (query "devel-1" (Slice (Attribute "name" "josh") Nothing))

  echo "Running a Scan command..."
  print =<< runCommand (scan "devel-1")

  echo "Filling table with several items..."
  forM_ [0..30] $ \ i -> do
    threadDelay 50000
    runCommand $ putItem "devel-1" $
      item [Attribute "name" (toValue $ T.pack ("lots-" ++ show i)), attrAs int "val" i]

  echo "Now paginating in increments of 5..."
  let q0 = (scan "devel-1") { sLimit = Just 5 }

  xs <- scanItemSource runCommand q0 $$ C.consume
  echo ("Pagination returned " ++ show (length xs) ++ " items")



runCommand r = do
    cfg <- Aws.baseConfiguration
    Aws.simpleAws cfg debugServiceConfig r

echo = putStrLn


