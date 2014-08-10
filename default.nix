{ pkgs ? import <nixpkgs> {} }:
let hask = pkgs.haskellPackages_ghc783.override { extension = self : super : {
}; };
in hask.callPackage ./aws.nix {}
