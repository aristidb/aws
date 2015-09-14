{ mkDerivation, base, either, safe, stdenv, transformers }:
mkDerivation {
  pname = "errors";
  version = "1.4.7";
  sha256 = "09g53dylwsw1phxq5zhkbq8pnpwqzipvqclmcrdypzkpwkmfncl7";
  buildDepends = [ base either safe transformers ];
  description = "Simplified error-handling";
  license = stdenv.lib.licenses.bsd3;
}
