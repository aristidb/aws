{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Aws.Ses.Model
    ( RawMessage(..)
    , Destination(..)
    , EmailAddress
    , Sender(..)
    , sesAsQuery
    ) where

import Data.ByteString.Char8 ({-IsString-})
import Data.Monoid
import Data.Text (Text)
import Data.Typeable

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE


class SesAsQuery a where
    -- | Write a data type as a list of query parameters.
    sesAsQuery :: a -> [(B.ByteString, B.ByteString)]

instance SesAsQuery a => SesAsQuery (Maybe a) where
    sesAsQuery = maybe [] sesAsQuery


-- | A raw e-mail.
data RawMessage = RawMessage { rawMessageData :: B.ByteString }
                deriving (Eq, Ord, Show, Typeable)

instance SesAsQuery RawMessage where
    sesAsQuery = (:[]) . (,) "RawMessage.Data" . B64.encode . rawMessageData


-- | The destinations of an e-mail.
data Destination =
    Destination
      { destinationBccAddresses :: [EmailAddress]
      , destinationCcAddresses  :: [EmailAddress]
      , destinationToAddresses  :: [EmailAddress]
      } deriving (Eq, Ord, Show, Typeable)

instance SesAsQuery Destination where
    sesAsQuery (Destination bcc cc to) = concat [ go (s "Bcc") bcc
                                                , go (s "Cc")  cc
                                                , go (s "To")  to ]
        where
          go kind = zipWith f (map Blaze8.fromShow [one..])
              where txt = kind `mappend` s "Addresses.member."
                    f n v = ( Blaze.toByteString (txt `mappend` n)
                            , TE.encodeUtf8 v )
          s = Blaze.fromByteString
          one = 1 :: Int

instance Monoid Destination where
    mempty = Destination [] [] []
    mappend (Destination a1 a2 a3) (Destination b1 b2 b3) =
        Destination (a1 ++ b1) (a2 ++ b2) (a3 ++ b3)


-- | An e-mail address.
type EmailAddress = Text


-- | The sender's e-mail address.
data Sender = Sender { senderAddress :: EmailAddress }
              deriving (Eq, Ord, Show, Typeable)

instance SesAsQuery Sender where
    sesAsQuery = (:[]) . (,) "Source" . TE.encodeUtf8 . senderAddress
