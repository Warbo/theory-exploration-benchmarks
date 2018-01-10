{ pkgsPath        ? null,              # For example <nixpkgs>
  pkgsArgs        ? { config = {}; },  # Args for 'import pkgsPath'
  haskellPackages ? null,
  nix-config-src  ? null,
  asv-nix         ? null }:

# Bring in the general builders, helpers, etc. that we need, allowing overrides.
with import ./pkgs.nix { inherit nix-config-src pkgsArgs pkgsPath; };

# Bring in bespoke definitions needed for this project
with callPackage ./racket.nix { inherit pkgs nixpkgs1609;          };
with callPackage ./tip.nix    { inherit nix-config racketWithPkgs; };
with callPackage ./cache.nix  { inherit env runRacket tip-repo;    };
with callPackage ./test.nix   { inherit cache env tip-repo;        };

# The definitions we expose
rec {
  inherit allTests env patchedHaskellPackages nix-config tests;

  # Used for benchmarking the benchmark generation (yo dawg)
  asv = if asv-nix == null
           then nix-config.asv-nix
           else asv-nix;

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
                                  testsPass = withDeps
                                    (attrValues (tests { full = false; }))
                                    nothing;
                                })
                                (./scripts + "/${n}.rkt"));
  };

  # The resulting benchmark, in various forms

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
