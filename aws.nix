{ cabal, aeson, attoparsec, base16Bytestring, base64Bytestring
, blazeBuilder, byteable, caseInsensitive, cereal, conduit
, conduitExtra, cryptohash, dataDefault, errors, filepath
, httpClient, httpConduit, httpTypes, liftedBase, monadControl, mtl
, network, QuickCheck, quickcheckInstances, resourcet, safe
, scientific, tagged, tasty, tastyQuickcheck, text, time
, transformers, transformersBase, unorderedContainers, utf8String
, vector, xmlConduit
}:

cabal.mkDerivation (self: {
  pname = "aws";
  version = "0.10.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base16Bytestring base64Bytestring blazeBuilder
    byteable caseInsensitive cereal conduit conduitExtra cryptohash
    dataDefault filepath httpConduit httpTypes liftedBase monadControl
    mtl network resourcet safe scientific tagged text time transformers
    unorderedContainers utf8String vector xmlConduit
  ];
  testDepends = [
    aeson errors httpClient liftedBase monadControl mtl QuickCheck
    quickcheckInstances resourcet tagged tasty tastyQuickcheck text
    time transformers transformersBase
  ];
  meta = {
    homepage = "http://github.com/aristidb/aws";
    description = "Amazon Web Services (AWS) for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
