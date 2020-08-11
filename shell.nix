{ pkgs ? import <nixpkgs> {} }:
let
  scripts = [
    (pkgs.writeScriptBin "style-project" ''
      #!/usr/bin/env bash
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/app/src
      ${pkgs.haskellPackages.ormolu.bin}/bin/ormolu --mode inplace $(find . -name '*.hs')
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      ${pkgs.haskellPackages.ormolu.bin}/bin/ormolu --mode inplace Build.hs
    '')
    (pkgs.writeScriptBin "build-shake" ''
      #!/usr/bin/env bash
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      mkdir -p _shake
      ghc --make Build.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_shake -o _shake/build
    '')
    (pkgs.writeScriptBin "build-project" ''
      #!/usr/bin/env bash
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      build-shake
      _shake/build "$@"
    '')
    (pkgs.writeScriptBin "clean-project" ''
      #!/usr/bin/env bash
      build-shake
      _shake/build clean
    '')
    (pkgs.writeScriptBin "watch-project" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/
      ${pkgs.nodePackages.nodemon}/bin/nodemon -e hs,yaml \
        --watch app/src \
        --watch app/package.yaml \
        --exec "run-project"
    '')
    (pkgs.writeScriptBin "run-project" ''
      #!/usr/bin/env bash
      set -e
      PROJECT_ROOT=$(${pkgs.git}/bin/git rev-parse --show-toplevel)/
      cd $PROJECT_ROOT
      build-project
      cd $PROJECT_ROOT/_build/bundle/
      ./chat-chat $PROJECT_ROOT/oidc-config.json +RTS -N
    '')
  ];
  scriptsDerivation = pkgs.stdenv.mkDerivation {
    name = "scripts";
    phases = "installPhase";
    installPhase = ''
      mkdir -p $out/bin
    '' + (builtins.concatStringsSep "" (builtins.map (script: ''
      for f in $(ls -d ${script}/bin/*); do ln -s $f $out/bin; done
    '') scripts));
  };
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: with p; [shake shake-cabal hpack ormolu.bin]))
    zlib
    cabal-install
    pkgconfig
    scriptsDerivation
  ];
in
  pkgs.mkShell {
    buildInputs = buildInputs;
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    '';
  }
