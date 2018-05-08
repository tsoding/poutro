with import <nixpkgs> {}; {
    PoutroEnv = stdenv.mkDerivation {
        name = "PoutroEnv";
        buildInputs = [ ghc stack cabal-install ];
    };
}
