{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }: with pkgs;
let
  ghcWithDeps = pkgs.haskell.packages.${compiler}.ghcWithPackages
    ( ps: with ps; [ base pandoc containers ] );
  tex = with pkgs; texlive.combine {
    inherit (texlive)
      scheme-small
      xetex
      newunicodechar
      ;
  };
  fontsConf = makeFontsConf {
    fontDirectories = [ dejavu_fonts ];
  };
in
  pkgs.stdenv.mkDerivation {
    name = "wiwinwlh-env";
    buildInputs = [ ghcWithDeps tex ];
    FONTCONFIG_FILE = fontsConf;
    shellHook = ''
      export LANG=en_US.UTF-8
      eval $(egrep ^export ${ghcWithDeps}/bin/ghc)
    '';
  }
