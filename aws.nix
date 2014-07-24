{ cabal, aeson, base16Bytestring, base64Bytestring, blazeBuilder
, byteable, caseInsensitive, cereal, conduit, conduitExtra
, cryptohash, dataDefault, filepath, httpConduit, httpTypes
, liftedBase, monadControl, mtl, resourcet, text, time
, transformers, unorderedContainers, utf8String, vector, xmlConduit
}:

cabal.mkDerivation (self: {
  pname = "aws";
  version = "0.9";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base16Bytestring base64Bytestring blazeBuilder byteable
    caseInsensitive cereal conduit conduitExtra cryptohash dataDefault
    filepath httpConduit httpTypes liftedBase monadControl mtl
    resourcet text time transformers unorderedContainers utf8String
    vector xmlConduit
  ];
  meta = {
    homepage = "http://github.com/aristidb/aws";
    description = "Amazon Web Services (AWS) for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
