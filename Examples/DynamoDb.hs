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


  resp1 <- Aws.simpleAws cfg debugServiceConfig req1
  print resp1

  putStrLn "Getting the item back..."

  let req2 = getItem "devel-1" (hk "name" "josh")
  resp2 <- Aws.simpleAws cfg debugServiceConfig req2
  print resp2

  print =<< Aws.simpleAws cfg debugServiceConfig
    (updateItem "devel-1" (hk "name" "josh") [au "class" "awesome"])

  echo "Updating with false conditional."
  (print =<< Aws.simpleAws cfg debugServiceConfig
    (updateItem "devel-1" (hk "name" "josh") [au "class" "awesomer"])
      { uiExpect = Expects CondAnd [Expect "name" (DEq "john")] })
    `catch` (\ (e :: DdbError) -> echo ("Eating exception: " ++ show e))

  echo "Getting the item back..."
  print =<< Aws.simpleAws cfg debugServiceConfig req2


  echo "Updating with true conditional"
  print =<< Aws.simpleAws cfg debugServiceConfig
    (updateItem "devel-1" (hk "name" "josh") [au "class" "awesomer"])
      { uiExpect = Expects CondAnd [Expect "name" (DEq "josh")] }

  echo "Getting the item back..."
  print =<< Aws.simpleAws cfg debugServiceConfig req2



echo = putStrLn


