{ bash, haskellPackages, nix, racket, stdenv, writeScript }:

stdenv.mkDerivation (rec {
  name = "te-benchmark";
  src  = ./.;

  propagatedBuildInputs = [ bash haskellPackages.cabal-install nix racket ];

  NIX_PATH   = builtins.getEnv "NIX_PATH";
  NIX_REMOTE = builtins.getEnv "NIX_REMOTE";

  doCheck = true;
  checkPhase = ''
    ./test.sh
  '';

  installPhase = ''
    mkdir -p      "$out/lib"
    cp    *.rkt   "$out/lib/"
    cp    *.sh    "$out/lib/"
    cp -r modules "$out/lib/"

    mkdir -p "$out/bin"
    cp '${mkPkg}'
  '';

  # Wrapper around full_haskell_package, which is the "end result" of all these
  # packages
  mkPkg = writeScript "te-benchmark" ''
    #!/usr/bin/env bash
    set -e

    BASE=$(dirname "$(readlink -f "$0")")
    cd "$BASE/../lib/"

    ./full_haskell_package.sh "$@"
  '';
})
