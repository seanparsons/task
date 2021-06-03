{ nixpkgs ? import <nixpkgs> {} }:
let
  overriddenHaskellPackages = nixpkgs.pkgs.haskellPackages;
in
  overriddenHaskellPackages.callCabal2nix "task" (./app) {}
