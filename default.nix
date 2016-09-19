{ bash, buildEnv, fetchurl, haskellPackages, makeWrapper, python, racket,
  stdenv, writeScript }:

let propagatedBuildInputs = [
      bash haskellPackages.cabal-install python racket
      (haskellPackages.ghcWithPackages (hs: [
        hs.tip-lib hs.QuickCheck hs.quickspec hs.testing-feat
      ])) ];
    env = buildEnv {
      name  = "tip-bench-env";
      paths = propagatedBuildInputs;
    };
in rec {

  # Scripts for combining all TIP benchmarks into one
  te-benchmark = stdenv.mkDerivation (rec {
    name = "te-benchmark";
    src  = ./.;

    buildInputs = [ makeWrapper ];
    inherit propagatedBuildInputs;

    NIX_PATH   = builtins.getEnv "NIX_PATH";
    NIX_REMOTE = builtins.getEnv "NIX_REMOTE";

    installPhase = ''
      mkdir -p      "$out/lib"
      cp    *.sh    "$out/lib/"
      cp    *.rkt   "$out/lib/"
      cp    *.py    "$out/lib/"
      cp -r modules "$out/lib/"

      # Ensure tip is available
      wrapProgram "$out/lib/mk_signature.sh" --prefix PATH : "${env}/bin"

      mkdir -p    "$out/bin"
      for F in "$out/lib"/*.sh "$out/lib"/*.rkt "$out/lib"/*.py
      do
        NAME=$(basename "$F")
        echo -e "#!/usr/bin/env bash\ncd '$out/lib'\n'$F' \"\$@\"" > "$out/bin/$NAME"
      done
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
      find modules/tip-benchmarks/benchmarks/ -name "*.smt2" |
        ./mk_final_defs.sh | ./full_haskell_package.sh
    '';
  };
}
