{ pkgs ? import <nixpkgs> {},
  haskellPackages ? null }:

with {
  chosenHaskellPackages = if haskellPackages == null
                             then pkgs.haskellPackages
                             else haskellPackages;
};
with builtins;
with pkgs;
with lib;

let
  shellPipeline = fetchFromGitHub {
    owner  = "willghatch";
    repo   = "racket-shell-pipeline";
    rev    = "7ed9a75";
    sha256 = "06z5bhmvpdhy4bakh30fzha4s0xp2arjq8h9cyi65b1y18cd148x";
  };

  # Dependency of grommet
  grip =  fetchFromGitHub {
    owner  = "RayRacine";
    repo   = "grip";
    rev    = "ec498f6";
    sha256 = "06ax30r70sz2hq0dzyassczcdkpmcd4p62zx0jwgc2zp3v0wl89l";
  };

  grommet = fetchFromGitHub {
    owner  = "RayRacine";
    repo   = "grommet";
    rev    = "50f1b6a";
    sha256 = "1rb7i8jx7gg2rm5flnql0hja4ph11p7i38ryxd04yqw50l0xj59v";
  };

  racketWithDeps = deps: stdenv.mkDerivation {
    name = "racket-with-deps";

    buildInputs = [ makeWrapper racket ];

    inherit deps;
    buildCommand = ''
      # raco writes to HOME, so make sure that's included
      export HOME="$out/etc"
      mkdir -p "$HOME"

      # Each PKG should be a directory (e.g. pulled from git) containing
      # "collections" as sub-directories. For example if PKG should allow
      # (require utils/printing), it should contain PKG/utils/printing.rkt

      # Collect up all packages
      mkdir -p "$out/share/pkgs"
      for PKG in $deps
      do
        cp -r "$PKG" "$out/share/pkgs/"
      done

      # Make our copies mutable, so we can compile them in-place
      chmod +w -R  "$out/share/pkgs"

      # Register packages with raco
      for PKG in "$out/share/pkgs/"*
      do
        # raco is Racket's package manager, -D says "treat as a directory of
        # collections", which is how git repos seem to be arranged.
        raco link --user -D "$PKG"
      done

      # Compile registered packages
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

  env = buildEnv {
    name  = "tip-bench-env";
    paths = [
      bash
      chosenHaskellPackages.cabal-install
      (racketWithDeps [ grip grommet shellPipeline ])
      (chosenHaskellPackages.ghcWithPackages (hs: [
        hs.tip-lib
        hs.QuickCheck
        hs.quickspec
        hs.testing-feat
      ]))
    ];
  };

  tip-repo = fetchFromGitHub {
    owner  = "tip-org";
    repo   = "benchmarks";
    rev    = "fae25da";
    sha256 = "08zm9a8dlwqm6bnd5z8714j5365pklwh4lkgcnhq0ns1lq0njp3l";
  };

in rec {

  # Take from git, to keep things pristine and cacheable
  tip-benchmarks = runCommand "tip-benchmarks" { repo = tip-repo; } ''
    cp -r "$repo/benchmarks" "$out"
  '';

  tools = stdenv.mkDerivation (rec {
    name = "te-benchmark";
    src  = ./scripts;

    buildInputs = [ env makeWrapper ];

    installPhase = ''
      # Install Racket scripts
      mkdir -p    "$out/lib"
      cp    *.rkt "$out/lib/"

      # Compile Racket scripts to bytecode for speed
      raco make "$out/lib/"*.rkt

      # For each Racket script, add a wrapper to PATH, without the .rkt suffix
      mkdir -p "$out/bin"
      for F in "$out/lib"/*.rkt
      do
        NAME=$(basename "$F" .rkt)

        # Write a one-liner to invoke this script, since shebangs don't seem to
        # use the bytecode
        echo -e "#!/usr/bin/env bash\nexec racket '$F'" > "$out/bin/$NAME"
        chmod +x "$out/bin/$NAME"

        # Wrap the one-liner so we can provide an appropriate environment.
        # Set PLT_COMPILED_FILE_CHECK to avoid checking bytecode timestamps.
        wrapProgram "$out/bin/$NAME"           \
          --prefix PATH : "${env}/bin"         \
          --set PWD "$out/lib"                 \
          --set PLT_COMPILED_FILE_CHECK exists \
          --set BENCHMARKS_FALLBACK "${tip-benchmarks}"
      done
    '';

    # This tells the tests where to find the benchmarks. Only a subset of files
    # will be tested, to make things faster.
    BENCHMARKS_FALLBACK = "${tip-benchmarks}";

    # Check contracts while testing; it's disabled by default for being too slow
    PLT_TR_CONTRACTS = "1";

    # Tells the tests where to find data, like example inputs.
    TEST_DATA = "${./test-data}";

    # Allow testing to be skipped, as it can take a few minutes
    doCheck    = getEnv "SKIP_TESTS" == "";
    checkPhase = "raco test defs.rkt";

    # Sets up the environment for the scripts and tests, and informs the user
    shellHook = ''
      {
        echo "Setting BENCHMARKS_FALLBACK to ${tip-benchmarks}"
        echo "To use a different set of benchmarks, you can set BENCHMARKS"
        export BENCHMARKS_FALLBACK="${tip-benchmarks}"

        echo "Setting TEST_DATA to ${./test-data}"
        export TEST_DATA="${./test-data}"

        echo "NOTE: We don't check Racket contracts because it's slow."
        echo "To enable contract checking, set PLT_TR_CONTRACTS to 1"

        echo "You can run tests with 'raco test scripts/defs.rkt'"
        echo "Use PLT_TEST_REGEX env var to limit which test cases are run."

        echo "Log messages are suppressed during tests. Set DEBUG to see them."
      } 1>&2
    '';
  });

  # Runs tests against all TIP benchmarks, rather than the sub-set used in tools
  tests = stdenv.mkDerivation {
    name         = "tip-tools-tests";
    src          = ./scripts;
    buildInputs  = [ env ];
    buildCommand = ''
      raco test "$src/defs.rkt" && echo "pass" > "$out"
    '';

    # Setting BENCHMARKS during tests overrides BENCHMARKS_FALLBACK, and also
    # causes all files to be tested rather than a subset.
    BENCHMARKS          = "${tip-benchmarks}";
    BENCHMARKS_FALLBACK = "${tip-benchmarks}";
    PLT_TR_CONTRACTS    = "1";
    TEST_DATA           = "${./test-data}";
  };

  tip-benchmark-smtlib = stdenv.mkDerivation {
    name         = "tip-benchmark-smtlib";
    buildInputs  = [ tools ];
    buildCommand = ''
      find "${tip-benchmarks}" -name "*.smt2" | mk_final_defs > "$out"
    '';
  };

  tip-benchmark-haskell = stdenv.mkDerivation {
    name         = "tip-benchmarks-haskell";
    buildInputs  = [ tools ];
    buildCommand = ''
      export OUT_DIR="$out"
      mkdir "$OUT_DIR"
      full_haskell_package < "${tip-benchmark-smtlib}"
    '';
  };
}
