with (import <nixpkgs> {}).pkgs;
let
  pkg = haskell.packages.ghc821.callPackage ./. {};
in
  pkg.env
