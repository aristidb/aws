with (import <nixpkgs> {}).pkgs;
let
  pkg = haskellPackages.callPackage ./. {};
in
  pkg.env
