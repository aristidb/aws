0.22 series
-----------

-   0.22
    - Support GHC 8.8
    - Support network-3
    - Support http-client 0.6+
    - S3: add etag to PutObjectResponse
    - Add IAM group manipulation methods

0.21 series
-----------

-   0.21.1
    - S3: Add PutBucketVersioning command

-   0.21
    - S3: Make user DisplayName field optional (used in "GetBucket"
      among other places)
    - Use HTTP.getGlobalManager from http-client-tls by default (more
      efficient, and we have a transitive dependency on the package
      anyways)

0.20 series
-----------

-   0.20
    - Update to conduit 1.3 and http-conduit 2.3 (breaking API change
      due to removal of ResumableSource, which was used in public APIs)
    - S3: Fix to V2 string signing

0.19 series
-----------

-   0.19
    - Experimental support for V4 signing
    - Add "eu-west-2" endpoint for some services
    - Loosen http-conduit bounds

0.18 series
-----------

-   0.18
    -   Switch from cryptohash to cryptonite
    -   Loosen boundaries for http-types and conduit-extra

0.17 series
-----------

-   0.17.1
    -   Fix testsuite build

-   0.17
    -   HTTP proxy support
    -   DDB: Support for additional interfaces, bug fixes
    -   Relax version bounds

0.16 series
-----------

NOTES: 0.16 brings technically breaking changes, which should not affect
most users. I recommend using smart constructors and {} matching syntax
whenever possible when interacting with aws types.

-   0.16
    -   S3: Add support for versioning
    -   S3: [breaking change] Move version ID from UploadPartResponse to
        CompleteMultipartUpload.

0.15 series
-----------

NOTES: 0.15 brings technically breaking changes, but should not affect
most users.

-   0.15.1
    -   Support xml-conduit 1.4

-   0.15
    -   Drop support for time <1.5
    -   Support http-client 2.2
    -   Support directory 1.3
    -   Add upper bound on http-client in testsuite
    -   DynamoDB: Eliminate orphan instance that conflicted with aeson-1.0
    -   S3: Don't URI encode response header override query params when signing
    -   Use HTTP.newManager instead of deprecated HTTP.withManager
    -   Signing: Change date format from space-padding to zero-padding

0.14 series
-----------

NOTES: 0.14 brings potentially breaking changes

-   0.14
    -   transformers 0.5 support
    -   data-default 0.6 support (also in 0.13.1)
    -   time < 2.0 support
    -   General: Use `AWS_SESSION_TOKEN` if in environment for loading credentials
    -   General: loadCredentialsDefault fails gracefully if HOME is not set
    -   DDB: Add parseAttr combinator for parsing an attribute into a FromDynItem
    -   DDB: Expose the new DynBool type
    -   S3: Add ETag fields to get/head object

0.13 series
-----------

NOTE: 0.13 brings breaking changes compared to 0.12.1!

-   0.13.1
    -   data-default 0.6 support
-   0.13
    -   DDB: Add support for scanning an index
    -   DDB: Allow deleting an attribute on update
    -   DDB: !BREAKING! Add support for native boolean values
        via "Bool". Can read old values, and there's a compatibility
        wrapper OldBool that behaves exactly the same way it used to.
    -   DDB: Add support for Null, L (list) and M (map) data types.
    -   DDB: Support consistent reads in Scan requests
    -   IAM: Add list-mfa-devices command
    -   S3: Extend StorageClass to support arbitrary classes, and
        StandardInfrequentAccess
    -   S3: Add a Sink interface for multipart uploading
    -   S3: Performance improvement for chunkedConduit
    -   S3: Partial support for Google Nearline

0.12 series
-----------

-   0.12.1
    -   DDB: Fix eu-west-1, add eu-central-1
    -   attoparsec 0.13
    -   xml-conduit 1.3
-   0.12
    -   S3: Support for "Expect: 100-continue" (optional, technically
        API breaking)
    -   S3: Properly treat errors with a "301 Permanent Redirect" as
        errors and expose endpoint information

0.11 series
-----------

-   0.11.4
    -   Url-encode S3 object names in URLs
    -   filepath 1.4
    -   tagged 0.8.x
    -   limit errors to &lt;2 to avoid compatibility problems
-   0.11.3
    -   Support for blaze-builder 0.4
    -   Support for utf8-string 1.0
    -   New function: multipartUploadWithInitiator
    -   Fix issue in DynamoDB error parsing
    -   Ord instance for Aws.Core.Method
-   0.11.2
    -   Support for time 1.5 (we previously forgot to relax the upper
        bound in Cabal)
-   0.11.1
    -   Support time 1.5
    -   Fix duplicate sending of query when using PostQuery
-   0.11
    -   New functions for running AWS transactions
    -   Performance optimizations for DynamoDB and S3 MultiPartUpload
    -   New DynamoDB commands & features
    -   S3 endpoint eu-central-1

0.10 series
-----------

-   0.10.5
    -   support for conduit 1.2
-   0.10.4
    -   S3: support for multi-part uploads
    -   DynamoDB: fixes for JSON serialization WARNING: This includes
        making some fields in TableDescription Maybe fields, which
        is breaking. But DynamoDB support was and is also marked
        as EXPERIMENTAL.
    -   DynamoDB: TCP connection reuse where possible
        (improving performance)
    -   DynamoDB: Added test suite
    -   SES: support for additional regions
-   0.10.3
    -   fix bug introduced in 0.10.2 that broke SQS and IAM connections
        without STS
-   0.10.2
    -   support STS / IAM temporary credentials in all services
-   0.10
    -   \[EXPERIMENTAL!\] DynamoDB: support for
        creating/updating/querying and scanning items
    -   SQS: complete overhaul to support 2012-11-05 features
    -   SQS: test suite
    -   S3: use Maybe for 404 HEAD requests on objects instead of
        throwing a misleading exception
    -   S3: support of poAutoMakeBucket for Internet Archive users
    -   S3: implement GetBucketLocation
    -   S3: add South American region
    -   S3: allow specifying the Content-Type when copying objects
    -   core: fix typo in NoCredentialsException accessor

0.9 series
----------

-   0.9.4
    -   allow conduit 1.2
-   0.9.3
    -   fix performance regression for loadCredentialsDefault
    -   add generic makeCredentials function
    -   add S3 DeleteBucket operation
    -   add S3 NukeBucket example
    -   SES: use security token if enabled (should allow using it with
        IAM roles on EC2 instances)
-   0.9.2
    -   Support for credentials from EC2 instance metadata (only S3
        for now)
    -   aeson 0.8 compatibility
-   0.9.1
    -   Support for multi-page S3 GetBucket requests
    -   S3 GLACIER support
    -   Applicative instance for Response to conform to the
        Applicative-Monad Proposal
    -   Compatibility with transformers 0.4
-   0.9
    -   Interface changes:
        -   attempt and failure were deprecated, remove
        -   switch to new cryptohash interface
    -   updated version bounds of conduit and xml-conduit

0.8 series
----------

-   0.8.6
    -   move Instance metadata functions out of ResourceT to remove
        problem with exceptions-0.5 (this makes a fresh install of aws
        on a clean system possible again)
-   0.8.5
    -   compatibility with case-insensitive 1.2
    -   support for V4 signatures
    -   experimental support for DynamoDB
-   0.8.4
    -   compatibility with http-conduit 2.0
-   0.8.3
    -   compatibility with cryptohash 0.11
    -   experimental IAM support
-   0.8.2
    -   compatibility with cereal 0.4.x
-   0.8.1
    -   compatibility with case-insensitive 1.1
-   0.8.0
    -   S3, SQS: support for US-West2 (\#58)
    -   S3: GetObject now has support for Content-Range (\#22, \#50)
    -   S3: GetBucket now supports the "IsTruncated" flag (\#39)
    -   S3: PutObject now supports web page redirects (\#46)
    -   S3: support for (multi-object) DeleteObjects (\#47, \#56)
    -   S3: HeadObject now uses an actual HEAD request (\#53)
    -   S3: fixed signing issues for GetObject call (\#54)
    -   SES: support for many more operations (\#65, \#66, \#70, \#71,
        \#72, \#74)
    -   SES: SendRawEmail now correctly encodes destinations and allows
        multiple destinations (\#73)
    -   EC2: support fo Instance metadata (\#37)
    -   Core: queryToHttpRequest allows overriding "Date" for the
        benefit of Chris Dornan's Elastic Transcoder bindings (\#77)

0.7 series
----------

-   0.7.6.4
    -   CryptoHash update
-   0.7.6.3
    -   In addition to supporting http-conduit 1.9, it would seem nice
        to support conduit 1.0. Previously slipped through the radar.
-   0.7.6.2
    -   Support for http-conduit 1.9
-   0.7.6.1
    -   Support for case-insensitive 1.0 and http-types 0.8
-   0.7.6
    -   Parsing of SimpleDB error responses was too strict, fixed
    -   Support for cryptohash 0.8
    -   Failure 0.1 does not work with aws, stricter lower bound
-   0.7.5
    -   Support for http-conduit 1.7 and 1.8
-   0.7.1-0.7.4
    -   Support for GHC 7.6
    -   Wider constraints to support newer versions of various
        dependencies
    -   Update maintainer e-mail address and project categories in cabal
        file
-   0.7.0
    -   Change ServiceConfiguration concept so as to indicate in the
        type whether this is for URI-only requests (i.e. awsUri)
    -   EXPERIMENTAL: Direct support for iterated transaction, i.e. such
        where multiple HTTP requests might be necessary due to e.g.
        response size limits.
    -   Put aws functions in ResourceT to be able to safely return
        Sources and streams.
        -   simpleAws\* does not require ResourceT and converts streams
            into memory values (like ByteStrings) first.
    -   Log response metadata (level Info), and do not let all aws
        runners return it.
    -   S3:
        -   GetObject: No longer require a response consumer in the
            request, return the HTTP response (with the body as
            a stream) instead.
        -   Add CopyObject (PUT Object Copy) request type.
    -   Add Examples cabal flag for building code examples.
    -   Many more, small improvements.

0.6 series
----------

-   0.6.2
    -   Properly parse Last-Modified header in accordance with RFC 2616.
-   0.6.1
    -   Fix for MD5 encoding issue in S3 PutObject requests.
-   0.6.0
    -   API Cleanup
        -   General: Use Crypto.Hash.MD5.MD5 when a Content-MD5 hash is
            required, instead of ByteString.
        -   S3: Made parameter order to S3.putObject consistent
            with S3.getObject.
    -   Updated dependencies:
        -   conduit 0.5 (as well as http-conduit 1.5 and
            xml-conduit 1.0).
        -   http-types 0.7.
    -   Minor changes.
    -   Internal changes (notable for people who want to add more
        commands):
        -   http-types' new 'QueryLike' interface allows creating query
            lists more conveniently.

0.5 series
----------

0.5.0

:   New configuration system: configuration split into general and
    service-specific parts.

    Significantly improved API reference documentation.

    Re-organised modules to make library easier to understand.

    Smaller improvements.

0.4 series
----------

0.4.1
:   Documentation improvements.

0.4.0.1
:   Change dependency bounds to allow the transformers 0.3 package.

0.4.0
:   Update conduit to 0.4.0, which is incompatible with
    earlier versions.

0.3 series
----------

0.3.2
:   Add awsRef / simpleAwsRef request variants for those who prefer an
    `IORef` over a `Data.Attempt.Attempt` value. Also improve README and
    add simple example.


