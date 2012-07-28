r53-dyndns - AWS Route53 Usage Example
======================================

The program r53-dyndns is simple dynamic DNS client that uses a AWS Route53
hosted zone as backend DNS Server. It monitors the publically visible IP
address of the local machine (through http://api.externalip.net/ip/) and
maintains the correspodings A record in an AWS Route53 hosted zone to point
that IP.

Beside of being useful, the purpose of this program is to demonstrate the usage
of the Haskell AWS Route53 API.

INSTALLATION
============

```bash
cabal configure
cabal build
cabal install
```

USAGE
=====

For usage information type `r53-dyndns --help`.

RUNNING AS A SERVICE
====================

Edit the file `contrib/r53-dyndns.conf` according to your needs and copy to
`/etc/init/r53-dyndns.conf`.

