{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
import           Aws
import           Aws.DynamoDb.Commands
import           Aws.DynamoDb.Core
import           Control.Monad.Catch
-------------------------------------------------------------------------------



main :: IO ()
main = do
  {- Load configuration -}
  cfg <- Aws.baseConfiguration

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


runCommand r = do
    cfg <- Aws.baseConfiguration
    Aws.simpleAws cfg debugServiceConfig r

echo = putStrLn


