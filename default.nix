{ pkgs ? import <nixpkgs> {} }:
let hask = pkgs.haskellPackages_ghc782.override { extension = self : super : { transformers = self.transformers_0_4_1_0; }; };
in hask.callPackage ./aws.nix {}
