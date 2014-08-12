{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- |
-- Module: Utils
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: BSD3
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
-- Utils for Tests for Haskell AWS bindints
--
module Utils
(
-- * Parameters
  testDataPrefix

-- * General Utils
, sshow
, tryT
, retryT
, testData

, evalTestT
, evalTestTM
, eitherTOnceTest0
, eitherTOnceTest1
, eitherTOnceTest2

-- * Generic Tests
, test_jsonRoundtrip
, prop_jsonRoundtrip
) where

import Control.Concurrent (threadDelay)
import Control.Error
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.Monoid
import Data.Proxy
import Data.String
import qualified Data.Text as T
import Data.Typeable

import Test.QuickCheck.Property
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

-- -------------------------------------------------------------------------- --
-- Static Test parameters
--

-- | This prefix is used for the IDs and names of all entities that are
-- created in the AWS account.
--
testDataPrefix :: IsString a => a
testDataPrefix = "__TEST_AWSHASKELLBINDINGS__"

-- -------------------------------------------------------------------------- --
-- General Utils

tryT :: MonadIO m => IO a -> EitherT T.Text m a
tryT = fmapLT (T.pack . show) . syncIO

testData :: (IsString a, Monoid a) => a -> a
testData a = testDataPrefix <> a

retryT :: MonadIO m => Int -> EitherT T.Text m a -> EitherT T.Text m a
retryT i f = go 1
  where
    go x
        | x >= i = fmapLT (\e -> "error after " <> sshow x <> " retries: " <> e) f
        | otherwise = f `catchT` \_ -> do
            liftIO $ threadDelay (1000000 * min 60 (2^(x-1)))
            go (succ x)

sshow :: (Show a, IsString b) => a -> b
sshow = fromString . show

evalTestTM
    :: Functor f
    => String -- ^ test name
    -> f (EitherT T.Text IO a) -- ^ test
    -> f (PropertyM IO Bool)
evalTestTM name = fmap $
    (liftIO . runEitherT) >=> \r -> case r of
        Left e ->
            fail $ "failed to run test \"" <> name <> "\": " <> show e
        Right _ -> return True

evalTestT
    :: String -- ^ test name
    -> EitherT T.Text IO a -- ^ test
    -> PropertyM IO Bool
evalTestT name = runIdentity . evalTestTM name . Identity

eitherTOnceTest0
    :: String -- ^ test name
    -> EitherT T.Text IO a -- ^ test
    -> TestTree
eitherTOnceTest0 name test = testProperty name . once . monadicIO
    $ evalTestT name test

eitherTOnceTest1
    :: (Arbitrary a, Show a)
    => String -- ^ test name
    -> (a -> EitherT T.Text IO b)
    -> TestTree
eitherTOnceTest1 name test = testProperty name . once $ monadicIO
    . evalTestTM name test

eitherTOnceTest2
    :: (Arbitrary a, Show a, Arbitrary b, Show b)
    => String -- ^ test name
    -> (a -> b -> EitherT T.Text IO c)
    -> TestTree
eitherTOnceTest2 name test = testProperty name . once $ \a b -> monadicIO
    $ (evalTestTM name $ uncurry test) (a, b)

-- -------------------------------------------------------------------------- --
-- Generic Tests

test_jsonRoundtrip
    :: forall a . (Eq a, Show a, FromJSON a, ToJSON a, Typeable a, Arbitrary a)
    => Proxy a
    -> TestTree
test_jsonRoundtrip proxy = testProperty msg (prop_jsonRoundtrip :: a -> Property)
  where
    msg = "JSON roundtrip for " <> show typ
#if MIN_VERSION_base(4,7,0)
    typ = typeRep proxy
#else
    typ = typeOf (undefined :: a)
#endif

prop_jsonRoundtrip :: forall a . (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
prop_jsonRoundtrip a = either (const $ property False) (\(b :: [a]) -> [a] === b) $
    eitherDecode $ encode [a]

