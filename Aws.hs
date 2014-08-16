module Aws
( -- * Logging
  LogLevel(..)
, Logger
, defaultLog
  -- * Configuration
, Configuration(..)
, baseConfiguration
, dbgConfiguration
, Environment(..)
, closeEnvironment
, newDefaultEnvironment
, newDebugEnvironment
, withDefaultEnvironment
, withDebugEnvironment
  -- * Transaction runners
  -- ** Safe runners
, aws
, awsRef
, pureAws
, simpleAws
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
, readResponse
, readResponseIO
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
, ServiceConfigurationMap
, addServiceConfiguration
, NormalQuery
, UriOnlyQuery
  -- ** Expiration
, TimeInfo(..)
  -- * Transactions
, Transaction
, IteratedTransaction
  -- * Credentials
, Credentials(..)
, makeCredentials
, credentialsDefaultFile
, credentialsDefaultKey
, loadCredentialsFromFile
, loadCredentialsFromEnv
, loadCredentialsFromInstanceMetadata
, loadCredentialsFromEnvOrFile
, loadCredentialsFromEnvOrFileOrInstanceMetadata
, loadCredentialsDefault
)
where

import Aws.Aws
import Aws.Core
