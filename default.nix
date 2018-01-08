{ pkgsPath        ? null,
  pkgsArgs        ? { config = {}; },
  haskellPackages ? null,
  nix-config-src  ? null,
  asv-nix         ? null }:

with builtins;
with rec {
  # Known-good version of nixpkgs
  repo1609 = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "f22817d";
    sha256 = "1cx5cfsp4iiwq8921c15chn1mhjgzydvhdcmrvjmqzinxyz71bzh";
  };

  nixpkgs1609 = import repo1609 pkgsArgs;
};
with {
  pkgs = if pkgsPath == null
            then nixpkgs1609
            else import pkgsPath pkgsArgs;
};
with pkgs;
with lib;
with rec {
  # Custom packages, overrides, etc.
  nix-config =
    with rec {
      nix-config-src-default = fetchgit {
        url    = "http://chriswarbo.net/git/nix-config.git";
        rev    = "15e860d";
        sha256 = "18x4cq2cl8dmw8zkk4a4kryh53bj98n61ydj1472ywhmvkanw944";
      };

      config-src = if nix-config-src == null
                      then nix-config-src-default
                      else nix-config-src;
    };
    import (if pkgsPath == null then repo1609 else pkgsPath) (pkgsArgs // {
      config = import "${config-src}/config.nix";
    });

  inherit (nix-config) attrsToDirs fail replace withDeps wrap;

  inherit (nix-config.callPackage ./racket.nix { inherit pkgs nixpkgs1609; })
    compileRacketScript racketWithPkgs runRacket;

  # Take the given Haskell packages, but override some things which are known
  # to be broken on Hackage. TODO: Get upstream to upload non-broken packages!
  patchedHaskellPackages =
    with rec {
      hsPkgs = if haskellPackages == null
                  then pkgs.haskell.packages.ghc7103
                  else haskellPackages;

      overrides = self: super:
        genAttrs [ "tip-lib" "geniplate" ]
                 (name: self.callPackage (overriddenHaskell name) {});

      overriddenHaskell = name:
        nix-config.haskell.packages.ghc7103."${name}".src;
    };
    hsPkgs.override { inherit overrides; };

  tip-repo = fetchFromGitHub {
    owner  = "tip-org";
    repo   = "benchmarks";
    rev    = "fae25da";
    sha256 = "08zm9a8dlwqm6bnd5z8714j5365pklwh4lkgcnhq0ns1lq0njp3l";
  };

  # Generates all the intermediate steps of the transformation
  cache = rec {
    # Take benchmarks from git, but transform them to replace "built-in"
    # definitions like "Bool" and "<" with explicitly defined versions.
    BENCHMARKS = runRacket "tip-benchmarks" [ env ] { repo = tip-repo; } ''
      (require lib/strip-native)

      (define source
        (mk-source (getenv "repo")))

      (define destination
        (getenv "out"))

      (define input-files
        (tip-files-in source))

      ;; Generate actual output
      (for-each (process-tip-file! source destination)
                input-files)
    '';

    BENCHMARKS_CACHE = runRacket "benchmarks-cache" [ env ]
      {
        inherit BENCHMARKS BENCHMARKS_FINAL_BENCHMARK_DEFS
                BENCHMARKS_NORMALISED_DEFINITIONS;
      }
      ''
        (require lib/impure)
        (require lib/sampling)

        ;; Generates data about renaming, etc. which can be cached and re-used
        ;; to make sampling and querying quicker.
        (write-to-out (format "~s" (make-sampling-data)))
      '';

    BENCHMARKS_NORMALISED_THEOREMS = runRacket "normalised-theorems" [ env ]
      { inherit BENCHMARKS_CACHE BENCHMARKS BENCHMARKS_NORMALISED_DEFINITIONS; }
      ''
        ;; Replaced (assoc-get 'normalised-theorems (get-sampling-data))
        ;; Write normalised-theorems return value to BENCHMARKS_NORMALISED_THEOREMS
        (require lib/impure)
        (require lib/theorems)
        (write-to-out (format "~s" (normalised-theorems)))
      '';

    BENCHMARKS_NORMALISED_DEFINITIONS = runRacket "normalised-definitions"
      [ env ]
      { inherit BENCHMARKS; }
      ''
        (require lib/impure)
        (require lib/normalise)
        (write-to-out
          (format "~s"
            (gen-normed-and-replacements)))
      '';

    BENCHMARKS_FINAL_BENCHMARK_DEFS = runRacket "final-defs"
      [ env ]
      { inherit BENCHMARKS BENCHMARKS_NORMALISED_DEFINITIONS; }
      ''
        (require lib/impure)
        (require lib/normalise)
        (require lib/tip)
        (write-to-out
          (format "~s"
            (prepare (first (normed-and-replacements-cached)))))
      '';
  };

  runTestScript = { full ? false }: runCommand "run-test"
    # Check contracts while testing; it's disabled by default for being too slow
    ((if full then { PLT_TR_CONTRACTS = "1"; } else {}) // cache // {
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
  fullToolTest = runTestScript { full = true; };

  # Installs tools for translating, sampling, etc. the benchmark. These tools
  # get cached data baked into them, which makes them slow to install but fast
  # to run.
  tools = attrsToDirs {
    bin = genAttrs
      [
        "choose_sample" "conjectures_admitted_by_sample"
        "conjectures_for_sample" "decode" "eqs_to_json" "full_haskell_package"
        "precision_recall_eqs"
      (n: compileRacketScript n
            (cache // { testsPass = runTestScript { full = false; }; })
            (./scripts + "/${n}.rkt"));
      ]
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
