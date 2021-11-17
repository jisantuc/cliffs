{ nixpkgs ? import <nixpkgs> { }
, compiler ? "ghc8107"
, extraToolDeps ? [ ]
}:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./cliffs.nix { extraToolDeps = extraToolDeps; }
