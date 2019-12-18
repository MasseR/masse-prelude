{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.haskellPackages.callPackage ./. {}
