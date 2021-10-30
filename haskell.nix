with import <nixpkgs> { };

pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
  # actual project dependencies
  brick
  cmark-gfm
  command
  containers
  directory
  filepath
  microlens
  text
  vector
  vty
])
