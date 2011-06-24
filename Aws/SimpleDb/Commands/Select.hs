{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.Select
where

import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import           Aws.Xml
import           Control.Monad
import           Data.Maybe
import           Text.XML.Enumerator.Cursor (($//), (&|))
import qualified Data.ByteString.UTF8       as BU
import qualified Text.XML.Enumerator.Cursor as Cu

data Select
    = Select {
        sSelectExpression :: String
      , sConsistentRead :: Bool
      , sNextToken :: String
      }
    deriving (Show)

data SelectResponse
    = SelectResponse {
        srItems :: [Item [Attribute String]]
      , srNextToken :: Maybe String
      }
    deriving (Show)

select :: String -> Select
select expr = Select { sSelectExpression = expr, sConsistentRead = False, sNextToken = "" }

instance SignQuery Select where
    type Info Select = SdbInfo
    signQuery Select{..}
        = sdbSignQuery $
            [("Action", "Select"), ("SelectExpression", BU.fromString sSelectExpression)] ++
            (guard sConsistentRead >> [("ConsistentRead", awsTrue)]) ++
            (guard (not $ null sNextToken) >> [("NextToken", BU.fromString sNextToken)])

instance SdbFromResponse SelectResponse where
    sdbFromResponse cursor = do
      sdbCheckResponseType () "SelectResponse" cursor
      items <- sequence $ cursor $// Cu.laxElement "Item" &| readItem
      let nextToken = listToMaybe $ cursor $// elCont "NextToken"
      return $ SelectResponse items nextToken

instance Transaction Select (SdbResponse SelectResponse)
