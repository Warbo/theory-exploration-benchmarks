{ bash, fetchurl, haskellPackages, python, racket, stdenv, writeScript }:

let propagatedBuildInputs = [
      bash haskellPackages.cabal-install python racket
      (haskellPackages.ghcWithPackages (hs: [
        hs.tip-lib hs.QuickCheck hs.quickspec hs.testing-feat
      ])) ];

rec {

  # Scripts for combining all TIP benchmarks into one
  te-benchmark = stdenv.mkDerivation (rec {
    name = "te-benchmark";
    src  = ./.;

    inherit propagatedBuildInputs;

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

    inherit propagatedBuildInputs;

    src = ./.;

    buildCommand = ''
      source $stdenv/setup

      cd "$src"
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

    teBenchmark = te-benchmark;

    buildCommand = ''
      source $stdenv/setup
      set -e

      # Create combined benchmark
      cd "$teBenchmark/lib"

      find modules/tip-benchmarks/benchmarks -name "*.smt2" |
        ./mk_final_defs.sh > "$out"
    '';
  };

  # Uses tip-benchmark-smtlib to produce a Haskell package
  tip-benchmarks = stdenv.mkDerivation {
    name         = "tip-benchmarks";
    buildInputs  = [ te-benchmark ];
    teBenchmark  = te-benchmark;
    SMT_FILE     = tip-benchmark-smtlib;
    buildCommand = ''
      source $stdenv/setup
      set -e

      OUT_DIR="$out"
      mkdir -p "$OUT_DIR"
      export OUT_DIR

      # Create Haskell package
      cd "$teBenchmark/lib"
      ./full_haskell_package.sh "$@"
    '';
  };
}
