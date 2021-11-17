{ mkDerivation
, base
, brick
, cmark-gfm
, command
, containers
, directory
, filepath
, hpack
, hspec
, hspec-discover
, lib
, microlens
, optparse-applicative
, text
, vector
, vty
, extraToolDeps ? [ ]
}:
mkDerivation {
  pname = "cliffs";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    brick
    cmark-gfm
    command
    containers
    directory
    filepath
    microlens
    optparse-applicative
    text
    vector
    vty
  ];
  libraryToolDepends = [ hpack ] ++ extraToolDeps;
  executableHaskellDepends = [
    base
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
  ];
  testHaskellDepends = [
    base
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
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/jisantuc/cliffs#readme";
  license = lib.licenses.mit;
}
