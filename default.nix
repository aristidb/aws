{ pkgs ? import <nixpkgs> {} }:
let hask = pkgs.haskellPackages_ghc782.override { extension = self : super : {
}; };
in hask.callPackage ./aws.nix {}
