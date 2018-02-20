{ pkgsPath        ? null,              # For example <nixpkgs>
  pkgsArgs        ? { config = {}; },  # Args for 'import pkgsPath'
  haskellPackages ? null,
  nix-config-src  ? null,
  asv-nix         ? null }:

# Bring in the general builders, helpers, etc. that we need, allowing overrides.
with import ./pkgs.nix { inherit nix-config-src pkgsArgs pkgsPath; };

# Bring in bespoke definitions needed for this project
with callPackage ./racket.nix { inherit pkgs nixpkgs1609;                      };
with callPackage ./tip.nix    { inherit nix-config PLTCOLLECTS racketWithPkgs; };
with callPackage ./cache.nix  { inherit env runRacket tip-repo;                };
with callPackage ./test.nix   { inherit cache env tip-repo;                    };
with { envFunc = env; env = env {}; };

# The definitions we expose
rec {
  inherit cache env patchedHaskellPackages nix-config tests;

  # Useful for those wanting to import our libraries
  scripts = copyPathToStore ./scripts;

  # Used for benchmarking the benchmark generation (yo dawg)
  asv = if asv-nix == null
           then nix-config.asv-nix
           else asv-nix;

  # Installs tools for translating, sampling, etc. the benchmark. These tools
  # get cached data baked into them, which makes them slow to build but fast to
  # run.
  tools =
    with rec {
      compile = n: compileRacketScript n (cache // {
          # Put the test results in the environment, so the tests will run
          # before we start compiling anything. If the test fail, we abort early
          testsPass = withDeps (attrValues (tests { full = false; }))
                               nothing;
        })
        [ env ]
        (./scripts + "/${n}.rkt");

      scripts = attrsToDirs {
        bin = genAttrs [ "choose_sample" "conjectures_admitted_by_sample"
                         "conjectures_for_sample" "decode" "eqs_to_json"
                         "full_haskell_package" "precision_recall_eqs" ]
                       compile;
      };

      checks = runCommand "tool-checks" { buildInputs = [ fail scripts ]; } ''
        set -e

        OUT_DIR="$PWD" full_haskell_package < ${tip-benchmark-smtlib} ||
          fail "Didn't make Haskell package"

        mkdir "$out"
      '';
    };
    withDeps [ checks ] scripts;

  # The resulting benchmark, in various forms

  tip-benchmark-smtlib = runRacket "tip-benchmark-smtlib" envFunc cache ''
    (require lib/normalise)
    (require lib/impure)

    (write-to-out (mk-final-defs))
  '';

  tip-benchmark-haskell = runRacket "tip-benchmark-haskell" envFunc cache ''
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
