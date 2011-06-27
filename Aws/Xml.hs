{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
module Aws.Xml
where
  
import           Aws.Response
import           Control.Monad.IO.Class
import           Data.Enumerator              ((=$))
import           Data.IORef
import           Data.Monoid
import           Data.Typeable
import           Text.XML.Enumerator.Cursor
import qualified Control.Exception            as C
import qualified Control.Failure              as F
import qualified Data.ByteString              as B
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Network.HTTP.Types           as HTTP
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML

newtype XmlException = XmlException { xmlErrorMessage :: String }
    deriving (Show, Typeable)

instance C.Exception XmlException

elCont :: T.Text -> Cursor -> [String]
elCont name = laxElement name &/ content &| T.unpack

force :: F.Failure XmlException m => String -> [a] -> m a
force = Cu.force . XmlException

forceM :: F.Failure XmlException m => String -> [m a] -> m a
forceM = Cu.forceM . XmlException

readInt :: (F.Failure XmlException m, Num a) => String -> m a
readInt s = case reads s of
              [(n,"")] -> return $ fromInteger n
              _        -> F.failure $ XmlException "Invalid Integer"

xmlCursorIteratee ::
    (Monoid m)
    => (Cu.Cursor -> Response m a)
    -> IORef m
    -> HTTP.Status 
    -> HTTP.ResponseHeaders 
    -> En.Iteratee B.ByteString IO a
xmlCursorIteratee parse metadataRef _status _headers
    = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
         let cursor = Cu.fromDocument doc
         let Response metadata x = parse cursor
         liftIO $ tellMetadataRef metadataRef metadata
         case x of                                  
           Left err -> En.throwError err
           Right v -> return v
