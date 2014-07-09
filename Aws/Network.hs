module Aws.Network where

import Control.Exception
import Network.BSD (getProtocolNumber)
import Network.Socket

-- It may give false positives, but never false negatives.
hostAvailable :: String -> IO Bool
hostAvailable h = do
  -- addrInfo <- (addrAddress . head) `fmap` getAddrInfo Nothing (Just h) Nothing
  remote <- getProtocolNumber "icmp" >>= socket AF_INET Datagram
  addr <- (addrAddress . head) `fmap` getAddrInfo (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } )) (Just h) (Just "80")
  case addr of
    (SockAddrInet _ s) -> do
      let local = (SockAddrInet aNY_PORT s)
      v <- catch (bindSocket remote local >> sendTo remote "" local >> return True)
                 (\(_ :: SomeException) -> return False)
      sClose remote
      return v
    _ -> return False -- disconnected
