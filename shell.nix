with import <nixpkgs> { };

{ pkgs ? import <nixpkgs> { } }:
let
  haskellWithPackages = import ./haskell.nix;
in
pkgs.mkShell {
  name = "cliffs";
  buildInputs = [
    haskellWithPackages
    cabal-install
  ];
}
