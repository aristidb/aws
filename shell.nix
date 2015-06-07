with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, attoparsec, base, base16-bytestring
             , base64-bytestring, blaze-builder, byteable, bytestring
             , case-insensitive, cereal, conduit, conduit-extra, containers
             , cryptohash, data-default, directory, errors, filepath
             , http-client, http-conduit, http-types, lifted-base, monad-control
             , mtl, network, old-locale, QuickCheck, quickcheck-instances
             , resourcet, safe, scientific, stdenv, tagged, tasty
             , tasty-quickcheck, text, time, transformers, transformers-base
             , unordered-containers, utf8-string, vector, xml-conduit
             }:
             mkDerivation {
               pname = "aws";
               version = "0.12";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 aeson attoparsec base base16-bytestring base64-bytestring
                 blaze-builder byteable bytestring case-insensitive cereal conduit
                 conduit-extra containers cryptohash data-default directory filepath
                 http-conduit http-types lifted-base monad-control mtl network
                 old-locale resourcet safe scientific tagged text time transformers
                 unordered-containers utf8-string vector xml-conduit
               ];
               testDepends = [
                 aeson base bytestring errors http-client lifted-base monad-control
                 mtl QuickCheck quickcheck-instances resourcet tagged tasty
                 tasty-quickcheck text time transformers transformers-base
               ];
               homepage = "http://github.com/aristidb/aws";
               description = "Amazon Web Services (AWS) for Haskell";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
