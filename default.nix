{ bash, fetchurl, haskellPackages, python, racket, stdenv, writeScript }:

rec {

  # Scripts for combining all TIP benchmarks into one
  te-benchmark = stdenv.mkDerivation (rec {
    name = "te-benchmark";
    src  = ./.;

    propagatedBuildInputs = [ bash haskellPackages.cabal-install python racket
                              (haskellPackages.ghcWithPackages (hs: [
                                hs.tip-lib hs.QuickCheck hs.quickspec
                                hs.testing-feat
                              ])) ];

    NIX_PATH   = builtins.getEnv "NIX_PATH";
    NIX_REMOTE = builtins.getEnv "NIX_REMOTE";

    # Wrapper around full_haskell_package, which is the "end result" of all
    # these scripts. The scripts assume they're being run from their own
    # directory, so we find out what that is and 'cd' to it.
    mkPkg = writeScript "te-benchmark" ''
              #!/usr/bin/env bash
              set -e

              BASE=$(dirname "$(readlink -f "$0")")
              cd "$BASE/../lib/"

              ./full_haskell_package.sh "$@"
            '';

    installPhase = ''
      mkdir -p      "$out/lib"
      cp    *.rkt   "$out/lib/"
      cp    *.sh    "$out/lib/"
      cp    *.py    "$out/lib/"
      cp -r modules "$out/lib/"

      mkdir -p    "$out/bin"
      cp "$mkPkg" "$out/bin/fullTePkg"
      chmod +x    "$out/bin/"*
    '';
  });

  # Test suite takes ages, so keep it separate
  te-benchmark-tests = stdenv.mkDerivation {
    name = "te-tests";

    teBenchmark  = te-benchmark;

    buildCommand = ''
      source $stdenv/setup

      cd "$teBenchmark/lib"
      HOME="$PWD" ./test.sh || exit 1
      touch "$out"
    '';
  };

  # Uses te-benchmark to produce one big smtlib file
  tip-benchmark-smtlib = stdenv.mkDerivation {
    name        = "tip-benchmark-smtlib";
    buildInputs = [ te-benchmark ];

    getSig = writeScript "tip-smtlib" ''
               #!/usr/bin/env bash
               set -e

               BASE=$(dirname "$(readlink -f "$0")")
               cd "$BASE/../lib/"

               echo "$PWD/combined-benchmark.smt2"
             '';

    buildCommand = ''
      source $stdenv/setup
      set -e

      mkdir -p "$out/lib"
      mkdir -p "$out/bin"

      # Create combined benchmark
      cd "${te-benchmark}/lib"

      find modules/tip-benchmarks/benchmarks -name "*.smt2" |
        ./mk_final_defs.sh > "$out/lib/combined-benchmark.smt2"

      # Install command for accessing the benchmark
      cp "$getSig" "$out/bin/completeTipSig"
      chmod +x "$out/bin"/*
    '';

    # This is a fixed-output derivation; the output must be a regular
    # file with MD5 hash md5.
    outputHashMode = "flat";
    outputHashAlgo = "sha256";
    outputHash     = "01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b";
  };

  # Uses tip-benchmark-smtlib to produce a Haskell package
  tip-benchmarks = stdenv.mkDerivation {
    name         = "tip-benchmarks";
    buildInputs  = [ te-benchmark tip-benchmark-smtlib ];
    buildCommand = ''
      source $stdenv/setup
      set -e

      command -v completeTipSig || {
        echo "Don't have completeTipSig" 1>&2
        exit 1
      }

      F=$(completeTipSig)
      [[ -e "$F" ]] || {
        echo "Benchmarks file '$F' doesn't exist" 1>&2
        exit 1
      }

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
