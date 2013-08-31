{ cabal, attempt, base64Bytestring, blazeBuilder, caseInsensitive
, cereal, conduit, cryptoApi, cryptohash, cryptohashCryptoapi
, failure, filepath, httpConduit, httpTypes, liftedBase
, monadControl, mtl, resourcet, text, time, transformers
, utf8String, xmlConduit
}:

cabal.mkDerivation (self: {
  pname = "aws";
  version = "0.8.0";
  sha256 = "0d9iim0h7ya2b4i59887zapdwxy42ajacp62kg117z15r27j28z9";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attempt base64Bytestring blazeBuilder caseInsensitive cereal
    conduit cryptoApi cryptohash cryptohashCryptoapi failure filepath
    httpConduit httpTypes liftedBase monadControl mtl resourcet text
    time transformers utf8String xmlConduit
  ];
  meta = {
    homepage = "http://github.com/aristidb/aws";
    description = "Amazon Web Services (AWS) for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
