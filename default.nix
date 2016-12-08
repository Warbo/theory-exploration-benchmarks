{ pkgs ? import <nixpkgs> {} }:

with pkgs;
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

  env = buildEnv {
    name  = "tip-bench-env";
    paths = [
      bash
      haskellPackages.cabal-install
      (racketWithDeps [ shellPipeline ])
      (haskellPackages.ghcWithPackages (hs: [
        hs.tip-lib
        hs.QuickCheck
        hs.quickspec
        hs.testing-feat
      ]))
    ];
  };

in rec {
  tip-benchmarks = ./modules/tip-benchmarks/benchmarks;

  tools = stdenv.mkDerivation (rec {
    name = "te-benchmark";
    src  = ./scripts;

    buildInputs = [ makeWrapper env ];

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
  });

  tip-benchmark-smtlib = runCommand "mk-smtlib"
    {
      buildInputs = [ tools ];
    }
    ''
      find "${tip-benchmarks}" -name "*.smt2" | mk_final_defs.rkt > "$out"
    '';
}
