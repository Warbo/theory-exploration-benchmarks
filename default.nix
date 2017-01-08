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
      (racketWithDeps [ shellPipeline ])
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
      mkdir -p      "$out/lib"
      cp    *.sh    "$out/lib/"
      cp    *.rkt   "$out/lib/"

      mkdir -p    "$out/bin"
      for F in "$out/lib"/*.sh "$out/lib"/*.rkt
      do
        NAME=$(basename "$F")
        makeWrapper "$F" "$out/bin/$NAME" --prefix PATH : "${env}/bin" \
                                          --set PWD "$out/lib"
      done
    '';

    doCheck = true;
    checkPhase = ''
      # BENCHMARKS tells the tests where to find their data

      # PLT_TR_CONTRACTS enables contract checking, which is useful but slow

      BENCHMARKS="${tip-benchmarks}" PLT_TR_CONTRACTS=1 raco test defs.rkt
    '';

    shellHook = ''
      {
        echo "Setting BENCHMARKS to ${tip-benchmarks}"
        export BENCHMARKS="${tip-benchmarks}"

        echo "NOTE: Checking Racket contracts is slow so disabled by default"
        echo "To enable contract checking, set PLT_TR_CONTRACTS to 1"

        echo "Use PLT_TEST_REGEX to limit the cases run during testing"
      } 1>&2
    '';
  });

  tip-benchmark-smtlib = runCommand "mk-smtlib"
    {
      buildInputs = [ tools ];
    }
    ''
      find "${tip-benchmarks}" -name "*.smt2" | mk_final_defs.rkt > "$out"
    '';
}
