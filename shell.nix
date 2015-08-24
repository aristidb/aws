with (import <nixpkgs> {}).pkgs;
let
  pkg = haskellngPackages.callPackage ./. {};
in
  pkg.env
