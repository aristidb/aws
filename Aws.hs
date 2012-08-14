module Aws
( -- * Logging
  LogLevel(..)
, Logger
, defaultLog
  -- * Configuration
, Configuration(..)
, baseConfiguration
, dbgConfiguration
  -- * Transaction runners
  -- ** Safe runners
, aws
, awsRef
, simpleAws
, simpleAwsRef
  -- ** Unsafe runners
, unsafeAws
, unsafeAwsRef
  -- ** URI runners
, awsUri
  -- ** Iterated runners
--, awsIteratedAll
, awsIteratedSource
, awsIteratedList
  -- * Response
  -- ** Full HTTP response
, HTTPResponseConsumer
  -- ** Metadata in responses
, Response(..)
, ResponseMetadata
  -- ** Memory responses
, AsMemoryResponse(..)
  -- ** Exception types
, XmlException(..)
, HeaderException(..)
, FormException(..)
  -- * Query
  -- ** Service configuration
, ServiceConfiguration
, DefaultServiceConfiguration(..)
  -- ** Expiration
, TimeInfo(..)
  -- * Transactions
, Transaction
, IteratedTransaction
  -- * Credentials
, Credentials(..)
, credentialsDefaultFile
, credentialsDefaultKey
, loadCredentialsFromFile
, loadCredentialsFromEnv
, loadCredentialsFromEnvOrFile
, loadCredentialsDefault
)
where

import Aws.Aws
import Aws.Core
