module Aws
( -- * Logging
  LogLevel(..)
, Logger
, defaultLog
  -- * Configuration
, Configuration(..)
, ConfigurationFetch
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
  -- * Response
  -- ** Full HTTP response
, HTTPResponseConsumer
  -- ** Metadata in responses
, Response(..)
, ResponseMetadata
  -- * Query
  -- ** Service configuration
, ServiceConfiguration
  -- ** Expiration
, TimeInfo(..)
  -- * Transactions
, Transaction
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
