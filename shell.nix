{ nixpkgs ? <nixpkgs>, compiler ? "ghc843" }:
let
  pkgs = import nixpkgs { };
  ghcWithDeps = pkgs.haskell.packages.${compiler}.ghcWithPackages
    ( ps: with ps; [ base pandoc containers ] );
  tex = with pkgs; texlive.combine {
    inherit (texlive)
      scheme-small
      xetex
      ;
  };
in
  pkgs.stdenv.mkDerivation {
    name = "wiwinwlh-env";
    buildInputs = [ ghcWithDeps tex ];
    shellHook = ''
      export LANG=en_US.UTF-8
      eval $(egrep ^export ${ghcWithDeps}/bin/ghc)
    '';
  }
