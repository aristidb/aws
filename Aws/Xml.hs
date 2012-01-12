{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
module Aws.Xml
where

import           Aws.Response
import           Control.Monad.IO.Class
import           Data.Attempt                 (Attempt(..))
import           Data.Conduit                 (($$))
import           Data.IORef
import           Data.Monoid
import           Data.Typeable
import           Text.XML.Cursor
import qualified Control.Exception            as E
import qualified Control.Failure              as F
import qualified Data.Conduit                 as C
import qualified Data.Text                    as T
import qualified Text.XML.Cursor              as Cu
import qualified Text.XML                     as XML

newtype XmlException = XmlException { xmlErrorMessage :: String }
    deriving (Show, Typeable)

instance E.Exception XmlException

elContent :: T.Text -> Cursor -> [T.Text]
elContent name = laxElement name &/ content

elCont :: T.Text -> Cursor -> [String]
elCont name = laxElement name &/ content &| T.unpack

force :: F.Failure XmlException m => String -> [a] -> m a
force = Cu.force . XmlException

forceM :: F.Failure XmlException m => String -> [m a] -> m a
forceM = Cu.forceM . XmlException

textReadInt :: (F.Failure XmlException m, Num a) => T.Text -> m a
textReadInt s = case reads $ T.unpack s of
                  [(n,"")] -> return $ fromInteger n
                  _        -> F.failure $ XmlException "Invalid Integer"

readInt :: (F.Failure XmlException m, Num a) => String -> m a
readInt s = case reads s of
              [(n,"")] -> return $ fromInteger n
              _        -> F.failure $ XmlException "Invalid Integer"

xmlCursorConsumer ::
    (Monoid m)
    => (Cu.Cursor -> Response m a)
    -> IORef m
    -> HTTPResponseConsumer a
xmlCursorConsumer parse metadataRef _status _headers source
    = do doc <- source $$ XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         let Response metadata x = parse cursor
         liftIO $ tellMetadataRef metadataRef metadata
         case x of
           Failure err -> liftIO $ C.resourceThrow err
           Success v   -> return v
