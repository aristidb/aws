{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages_ghc783_profiling.callPackage ./aws.nix {}
