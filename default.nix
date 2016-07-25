{ bash, haskellPackages, racket, stdenv, writeScript }:

# Wrapper around full_haskell_package, which is the "end result" of all
# these scripts
let mkPkg = writeScript "te-benchmark" ''
              #!/usr/bin/env bash
              set -e

              BASE=$(dirname "$(readlink -f "$0")")
              cd "$BASE/../lib/"

              ./full_haskell_package.sh "$@"
            '';
 in rec {

  te-benchmark = stdenv.mkDerivation (rec {
    name = "te-benchmark";
    src  = ./.;

    propagatedBuildInputs = [ bash haskellPackages.cabal-install racket
                              (haskellPackages.ghcWithPackages (hs: [
                                hs.tip-lib hs.QuickCheck hs.quickspec
                                hs.testing-feat
                              ])) ];

    NIX_PATH   = builtins.getEnv "NIX_PATH";
    NIX_REMOTE = builtins.getEnv "NIX_REMOTE";

    doCheck = true;
    checkPhase = ''
      HOME="$PWD" ./test.sh
    '';

    installPhase = ''
      mkdir -p      "$out/lib"
      cp    *.rkt   "$out/lib/"
      cp    *.sh    "$out/lib/"
      cp -r modules "$out/lib/"

      mkdir -p "$out/bin"
      cp "${mkPkg}" "$out/bin/fullTePkg"
      chmod +x "$out/bin/"*
    '';
  });

  tip-benchmarks = stdenv.mkDerivation {
    name         = "tip-benchmarks";
    buildInputs  = [ te-benchmark ];
    buildCommand = ''
      source $stdenv/setup
      set -e

      # fullTePkg expects OUT_DIR env var
      OUT_DIR="$PWD/tip-benchmark-sig"
      mkdir -p "$OUT_DIR"
      export OUT_DIR

      # Create Haskell package
      fullTePkg

      # Store Haskell package
      cp -a "$OUT_DIR" "$out"
    '';
  };
}
