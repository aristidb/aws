Introduction
============

The `aws` package attempts to provide support for using Amazon Web
Services like S3 (storage), SQS (queuing) and others to Haskell
programmers. The ultimate goal is to support all Amazon Web Services.

Installation
============

Make sure you have a recent GHC installed, as well as cabal-install, and
installation should be as easy as:

``` {.bash}
$ cabal install aws
```

If you prefer to install from source yourself, you should first get a
clone of the `aws` repository, and install it from inside the source
directory:

``` {.bash}
$ git clone https://github.com/aristidb/aws.git
$ cd aws
$ cabal install
```

Using aws
=========

Concepts and organisation
-------------------------

The aws package is organised into the general `Aws` module namespace,
and subnamespaces like `Aws.S3` for each Amazon Web Service. Under each
service namespace in turn, there are general support modules and and
`Aws.<Service>.Commands.<Command>` module for each command. For easier
usage, there are the "bundling" modules `Aws` (general support), and
`Aws.<Service>`.

The primary concept in aws is the *Transaction*, which corresponds to a
single HTTP request to the Amazon Web Services. A transaction consists
of a request and a response, which are associated together via the
`Transaction` typeclass. Requests and responses are simple Haskell
records, but for some requests there are convenience functions to fill
in default values for many parameters.

Example usage
-------------

To be able to access AWS resources, you should put your into a
configuration file. (You don't have to store it in a file, but that's
how we do it in this example.) Save the following in `$HOME/.aws-keys`.

``` {.example}
default AccessKeyID SecretKey
```

You do have to replace AccessKeyID and SecretKey with the Access Key ID
and the Secret Key respectively, of course.

Then, copy this example into a Haskell file, and run it with `runghc`
(after installing aws):

``` {.haskell}
{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (withManager, responseBody)

main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    {- Create a request object with S3.getObject and run the request with pureAws. -}
    S3.GetObjectResponse { S3.gorResponse = rsp } <-
      Aws.pureAws cfg s3cfg mgr $
        S3.getObject "haskell-aws" "cloud-remote.pdf"

    {- Save the response to a file. -}
    responseBody rsp $$+- sinkFile "cloud-remote.pdf"
```

You can also find this example in the source distribution in the
`Examples/` folder.

Frequently Asked Questions
==========================

S3 questions
------------

-   I get an error when I try to access my bucket with upper-case
    characters / a very long name.

    Those names are not compliant with DNS. You need to use path-style
    requests, by setting `s3RequestStyle` in the configuration to
    `PathStyle`. Note that such bucket names are only allowed in the US
    standard region, so your endpoint needs to be US standard.

Release Notes
=============

See CHANGELOG

Resources
=========

-   [aws on Github](https://github.com/aristidb/aws)
-   [aws on Hackage](http://hackage.haskell.org/package/aws) (includes
    reference documentation)
-   [Official Amazon Web Services website](http://aws.amazon.com/)

Contributors
============

  Name                |Github                                           |E-Mail                          |Company                                              |Components
  --------------------|-------------------------------------------------|--------------------------------|-----------------------------------------------------|--------------------
  Abhinav Gupta       |[abhinav](https://github.com/abhinav)            |mail@abhinavg.net               |-                                                    |IAM, SES
  Aristid Breitkreuz  |[aristidb](https://github.com/aristidb)          |aristidb@gmail.com              |-                                                    |Co-Maintainer
  Bas van Dijk        |[basvandijk](https://github.com/basvandijk)      |v.dijk.bas@gmail.com            |[Erudify AG](http://erudify.ch)                      |S3
  David Vollbracht    |[qxjit](https://github.com/qxjit)                |                                |                                                     |
  Felipe Lessa        |[meteficha](https://github.com/meteficha)        |felipe.lessa@gmail.com          |currently secret                                     |Core, S3, SES
  Nathan Howell       |[NathanHowell](https://github.com/NathanHowell)  |nhowell@alphaheavy.com          |[Alpha Heavy Industries](http://www.alphaheavy.com)  |S3
  Ozgun Ataman        |[ozataman](https://github.com/ozataman)          |ozgun.ataman@soostone.com       |[Soostone Inc](http://soostone.com)                  |Core, S3, DynamoDb
  Steve Severance     |[sseveran](https://github.com/sseveran)          |sseverance@alphaheavy.com       |[Alpha Heavy Industries](http://www.alphaheavy.com)  |S3, SQS
  John Wiegley        |[jwiegley](https://github.com/jwiegley)          |johnw@fpcomplete.com            |[FP Complete](http://fpcomplete.com)                 |Co-Maintainer, S3
  Chris Dornan        |[cdornan](https://github.com/cdornan)            |chris.dornan@irisconnect.co.uk  |[Iris Connect](http://irisconnect.co.uk)             |Core
  John Lenz           |[wuzzeb](https://github/com/wuzzeb)              |                                |                                                     |DynamoDB, Core


