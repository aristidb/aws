{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages_ghc784_profiling.callPackage ./aws.nix {}
