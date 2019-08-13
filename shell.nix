{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  hpkgs = haskellPackages;
  btools = [
    hpkgs.cabal-install
    ghcid
    hpkgs.hlint
    hpkgs.hasktags
    hpkgs.haskdogs
    google-cloud-sdk
  ];
  modifier = drv: haskell.lib.addBuildTools drv btools;
in
  haskellPackages.developPackage { root = ./.; inherit modifier; }
