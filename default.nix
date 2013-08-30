{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./aws.nix {}
