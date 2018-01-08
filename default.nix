{ pkgsPath        ? null,
  pkgsArgs        ? { config = {}; },
  haskellPackages ? null,
  nix-config-src  ? null,
  asv-nix         ? null }:

# Bring in the general builders, helpers, etc. that we need, allowing overrides.
with builtins;
with import ./pkgs.nix { inherit nix-config-src pkgsArgs pkgsPath; };
with pkgs;
with lib;
with {
  inherit (nix-config) attrsToDirs callPackage fail replace withDeps wrap;
};

# Bring in bespoke definitions needed for this project

with rec {
  inherit (callPackage ./racket.nix { inherit pkgs nixpkgs1609; })
    compileRacketScript racketWithPkgs runRacket;

  inherit (callPackage ./tip.nix    { inherit nix-config; })
    patchedHaskellPackages tip-repo;

  inherit (callPackage ./cache.nix  { inherit env runRacket tip-repo; })
    cache;

  tests = { full ? false }: runCommand "run-tests"
    # Check contracts while testing; it's disabled by default for being too slow
    (cache // {
      PLT_TR_CONTRACTS  = if full then "1" else null;

      TEST_DATA         = ./test-data/nat-simple-raw.json;
      TEST_LIST_EQS     = ./test-data/list-full-found.json;
      TEST_LIST_TRUTH   = ./test-data/list-full-ground-truth.smt2;
      BENCHMARKS_SOURCE = tip-repo + "/benchmarks";

      buildInputs       = [ env fail ];
      testScript        = writeScript "run-tests.rkt" ''
        #lang racket
        (require (submod lib/impure       test))
        (require (submod lib/compare      test))
        (require (submod lib/lists        test))
        (require (submod lib/sets         test))
        (require (submod lib/defs         test))
        (require (submod lib/util         test))
        (require (submod lib/strip-native test))
        (require (submod lib/replacements test))
        (require (submod lib/tip          test))
        (require (submod lib/normalise    test))
        (require (submod lib/theorems     test))
        (require (submod lib/sigs         test))
        (require (submod lib/sampling     test))
        (require (submod lib/conjectures  test))
        (module+ test)
      '';
    })
    ''
      set -e
      raco test "$testScript" || fail "Tests failed"
      mkdir "$out"
    '';

  env = buildEnv {
    name  = "tip-bench-env";
    paths = [
      bash
      patchedHaskellPackages.cabal-install
      racketWithPkgs
      (patchedHaskellPackages.ghcWithPackages (hs: [
        hs.tip-lib
        hs.geniplate
        hs.QuickCheck
        hs.quickspec
        hs.testing-feat
        hs.cereal
        hs.murmur-hash
      ]))
    ];
  };
};
rec {
  inherit env patchedHaskellPackages nix-config;

  # Used for benchmarking the benchmark generation (yo dawg)
  asv = if asv-nix == null
           then nix-config.asv-nix
           else asv-nix;

  # Standalone since it's too slow to use as a dependency of tools
  fullToolTest = tests { full = true; };

  # Installs tools for translating, sampling, etc. the benchmark. These tools
  # get cached data baked into them, which makes them slow to build but fast to
  # run.
  tools = attrsToDirs {
    bin = genAttrs
      [
        "choose_sample" "conjectures_admitted_by_sample"
        "conjectures_for_sample" "decode" "eqs_to_json" "full_haskell_package"
        "precision_recall_eqs"
      ]
      (n: compileRacketScript n (cache // {
                                  # Put the test results in the environment, so
                                  # the tests will run before we start compiling
                                  # anything. If the test fail, we abort early.
                                  testsPass = tests { full = false; };
                                })
                                (./scripts + "/${n}.rkt"));
  };

  tip-benchmark-smtlib = runRacket "tip-benchmark-smtlib" [] cache ''
    (require lib/normalise)
    (require lib/impure)

    (write-to-out (mk-final-defs))
  '';

  tip-benchmark-haskell = runRacket "tip-benchmark-haskell" [ env ] cache ''
    (require lib/impure)
    (require lib/normalise)
    (require lib/sigs)
    (require lib/util)

    (define out (getenv "out"))
    (make-directory* out)

    (parameterize-env `([#"OUT_DIR" ,(string->bytes/utf-8 out)])
                      (lambda ()
                        (full-haskell-package-s
                          (format-symbols (final-benchmark-defs))
                          out)))
  '';
}
