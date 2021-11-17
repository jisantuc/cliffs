{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc8107" }:
(import ./default.nix { extraToolDeps = [ nixpkgs.cabal-install ]; inherit nixpkgs compiler; }).env
