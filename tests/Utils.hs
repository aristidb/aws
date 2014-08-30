{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

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
, retryT_
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

import Control.Applicative
import Control.Concurrent (threadDelay)
import qualified Control.Exception.Lifted as LE
import Control.Error hiding (syncIO)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.Dynamic (Dynamic)
import Data.Monoid
import Data.Proxy
import Data.String
import qualified Data.Text as T
import Data.Typeable

import Test.QuickCheck.Property
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import System.Exit (ExitCode)

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

-- | Catches all exceptions except for asynchronous exceptions found in base.
--
tryT :: MonadBaseControl IO m => m a -> EitherT T.Text m a
tryT = fmapLT (T.pack . show) . syncIO

-- | Lifted Version of 'syncIO' form "Control.Error.Util".
--
syncIO :: MonadBaseControl IO m => m a -> EitherT LE.SomeException m a
syncIO a = EitherT $ LE.catches (Right <$> a)
    [ LE.Handler $ \e -> LE.throw (e :: LE.ArithException)
    , LE.Handler $ \e -> LE.throw (e :: LE.ArrayException)
    , LE.Handler $ \e -> LE.throw (e :: LE.AssertionFailed)
    , LE.Handler $ \e -> LE.throw (e :: LE.AsyncException)
    , LE.Handler $ \e -> LE.throw (e :: LE.BlockedIndefinitelyOnMVar)
    , LE.Handler $ \e -> LE.throw (e :: LE.BlockedIndefinitelyOnSTM)
    , LE.Handler $ \e -> LE.throw (e :: LE.Deadlock)
    , LE.Handler $ \e -> LE.throw (e ::    Dynamic)
    , LE.Handler $ \e -> LE.throw (e :: LE.ErrorCall)
    , LE.Handler $ \e -> LE.throw (e ::    ExitCode)
    , LE.Handler $ \e -> LE.throw (e :: LE.NestedAtomically)
    , LE.Handler $ \e -> LE.throw (e :: LE.NoMethodError)
    , LE.Handler $ \e -> LE.throw (e :: LE.NonTermination)
    , LE.Handler $ \e -> LE.throw (e :: LE.PatternMatchFail)
    , LE.Handler $ \e -> LE.throw (e :: LE.RecConError)
    , LE.Handler $ \e -> LE.throw (e :: LE.RecSelError)
    , LE.Handler $ \e -> LE.throw (e :: LE.RecUpdError)
    , LE.Handler $ return . Left
    ]

testData :: (IsString a, Monoid a) => a -> a
testData a = testDataPrefix <> a

retryT :: MonadIO m => Int -> EitherT T.Text m a -> EitherT T.Text m a
retryT n f = snd <$> retryT_ n f

retryT_ :: MonadIO m => Int -> EitherT T.Text m a -> EitherT T.Text m (Int, a)
retryT_ n f = go 1
  where
    go x
        | x >= n = fmapLT (\e -> "error after " <> sshow x <> " retries: " <> e) ((x,) <$> f)
        | otherwise = ((x,) <$> f) `catchT` \_ -> do
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

