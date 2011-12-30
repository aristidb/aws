{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Aws.SimpleDb.Commands.Select
where

import           Aws.Response
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Metadata
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import           Aws.Xml
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Text.XML.Cursor            (($//), (&|))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Text.XML.Cursor            as Cu

data Select
    = Select {
        sSelectExpression :: T.Text
      , sConsistentRead :: Bool
      , sNextToken :: Maybe T.Text
      }
    deriving (Show)

data SelectResponse
    = SelectResponse {
        srItems :: [Item [Attribute T.Text]]
      , srNextToken :: Maybe T.Text
      }
    deriving (Show)

select :: T.Text -> Select
select expr = Select { sSelectExpression = expr, sConsistentRead = False, sNextToken = Nothing }

instance SignQuery Select where
    type Info Select = SdbInfo
    signQuery Select{..}
        = sdbSignQuery . catMaybes $
            [ Just ("Action", "Select")
            , Just ("SelectExpression", T.encodeUtf8 sSelectExpression)
            , ("ConsistentRead", awsTrue) <$ guard sConsistentRead
            , (("NextToken",) . T.encodeUtf8) <$> sNextToken
            ]

instance ResponseConsumer r SelectResponse where
    type ResponseMetadata SelectResponse = SdbMetadata
    responseConsumer _ = sdbResponseConsumer parse
        where parse cursor = do
                sdbCheckResponseType () "SelectResponse" cursor
                items <- sequence $ cursor $// Cu.laxElement "Item" &| readItem
                let nextToken = listToMaybe $ cursor $// elContent "NextToken"
                return $ SelectResponse items nextToken

instance Transaction Select SelectResponse
