{ cabal, attempt, base64Bytestring, blazeBuilder, caseInsensitive
, cereal, conduit, cryptoApi, cryptohash, cryptohashCryptoapi
, failure, filepath, httpConduit, httpTypes, liftedBase
, monadControl, mtl, resourcet, text, time, transformers
, utf8String, xmlConduit
}:

cabal.mkDerivation (self: {
  pname = "aws";
  version = "0.8";
  sha256 = "1fdmf62xsmhljh3wdl49qsyf7ipy6gb9bzvqkjk81asqzaqgyrsh";
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
