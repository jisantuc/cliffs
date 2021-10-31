with import <nixpkgs> { };

pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
  # actual project dependencies
  brick
  cmark-gfm
  command
  containers
  directory
  filepath
  hspec
  hspec-discover
  microlens
  text
  vector
  vty
])
