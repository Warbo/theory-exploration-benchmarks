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

    doCheck    = getEnv "SKIP_TESTS" == "";
    checkPhase = ''
      # BENCHMARKS tells the tests where to find their data

      # PLT_TR_CONTRACTS enables contract checking, which is useful but slow

      BENCHMARKS="${tip-benchmarks}" PLT_TR_CONTRACTS=1 raco test defs.rkt
    '';

    shellHook = ''
      {
        echo "Setting BENCHMARKS_FALLBACK env varto ${tip-benchmarks}"
        echo "This can be overridden by providing a BENCHMARKS env var"
        export BENCHMARKS_FALLBACK="${tip-benchmarks}"

        echo "NOTE: We don't check Racket contracts because it's slow."
        echo "To enable contract checking, set PLT_TR_CONTRACTS env var to 1"

        echo "Test with 'raco test scripts/defs.rkt'"
        echo "Use PLT_TEST_REGEX env var to limit the test cases which are run."

        echo "For more information during tests, set DEBUG env var"
      } 1>&2
    '';
  });

  tip-benchmark-smtlib = runCommand "mk-smtlib"
    {
      buildInputs = [ tools ];
    }
    ''
      find "${tip-benchmarks}" -name "*.smt2" | mk_final_defs > "$out"
    '';

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
