with import <nixpkgs> { };

pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
  # actual project dependencies
  brick
  directory
  filepath
  microlens
  text
  vector
  vty
])
