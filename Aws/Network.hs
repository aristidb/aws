module Aws.Network where

import Data.Maybe
import Control.Exception
import Network.BSD (getProtocolNumber)
import Network.Socket
import System.Timeout

-- It may give false positives, but never false negatives.
hostAvailable :: String -> IO Bool
hostAvailable h = do
  sock <- getProtocolNumber "tcp" >>= socket AF_INET Stream
  addr <- (addrAddress . head) `fmap` getAddrInfo (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } )) (Just h) (Just "80")
  case addr of
    remote@(SockAddrInet _ _) -> do
      v <- catch (timeout 100000 (connect sock remote) >>= return . isJust)
                 (\(_ :: SomeException) -> return False)
      sClose sock
      return v
    _ -> return False
