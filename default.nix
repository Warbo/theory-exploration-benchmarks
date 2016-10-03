{ bash, buildEnv, fetchurl, fetchFromGitHub, haskellPackages,
  makeWrapper, python, racket, stdenv, writeScript }:

let propagatedBuildInputs = [
      bash haskellPackages.cabal-install python
      (racketWithDeps [ shellPipeline ])
      (haskellPackages.ghcWithPackages (hs: [
        hs.tip-lib hs.QuickCheck hs.quickspec hs.testing-feat
      ])) ];
    env = buildEnv {
      name  = "tip-bench-env";
      paths = propagatedBuildInputs;
    };

  shellPipeline = fetchFromGitHub {
    owner  = "willghatch";
    repo   = "racket-shell-pipeline";
    rev    = "7ed9a75";
    sha256 = "06z5bhmvpdhy4bakh30fzha4s0xp2arjq8h9cyi65b1y18cd148x";
  };

  racketWithDeps = deps: stdenv.mkDerivation {
    name = "racket-with-deps";

    buildInputs = [ makeWrapper racket ];
    racketDeps  = deps;

    buildCommand = ''
      # raco writes to HOME, so make sure that's included
      export HOME="$out/etc"
      mkdir -p "$HOME"

      # Each PKG should be a directory (e.g. pulled from git) containing
      # "collections" as sub-directories. For example if PKG should allow
      # (require utils/printing), it should contain PKG/utils/printing.rkt
      mkdir -p "$out/share/pkgs"
      for PKG in $racketDeps
      do
        # Make a mutable copy of the package, so we can compile it
        NAME=$(basename "$PKG")
        cp -r "$PKG" "$out/share/pkgs/$NAME"
        chmod +w -R  "$out/share/pkgs/$NAME"

        # raco is Racket's package manager, -D says "treat as a directory of
        # collections", which is how git repos seem to be arranged.
        raco link --user -D "$out/share/pkgs/$NAME"
      done

      # Compile packages
      raco setup --avoid-main -x -D

      # Provide Racket binaries patched to use our modified HOME
      mkdir -p "$out/bin"
      for PROG in "${racket}"/bin/*
      do
        NAME=$(basename "$PROG")
        makeWrapper "$PROG" "$out/bin/$NAME" --set HOME "$out/etc"
      done
    '';
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
      wrapProgram "$out/lib/mk_signature.rkt" --prefix PATH : "${env}/bin"

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

    teBenchmark = te-benchmark;

    buildCommand = ''
      source $stdenv/setup
      set -e

      # Create combined benchmark
      cd "$teBenchmark/lib"

      find modules/tip-benchmarks/benchmarks -name "*.smt2" |
        ./mk_final_defs.rkt > "$out"
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
        ./mk_final_defs.rkt | ./full_haskell_package.rkt
    '';
  };
}
