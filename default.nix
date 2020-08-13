{ nixpkgs ? import <nixpkgs> {} }:
let
  overriddenHaskellPackages = nixpkgs.pkgs.haskellPackages.override {
    overrides = self: super: rec {
    table-layout  = self.callCabal2nix "table-layout" (builtins.fetchGit {
        url = "git@github.com:muesli4/table-layout.git";
        rev = "125584715d44729952af4b9558f7ca79dbd75fca";
      })
      {};
    };
  };
in
  overriddenHaskellPackages.callCabal2nix "task" (./app) {}
