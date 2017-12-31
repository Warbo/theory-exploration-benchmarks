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

  # Racket may be disabled ( https://github.com/NixOS/nixpkgs/pull/23542 )
  workingRacket =
    with (tryEval pkgs.racket);
    if success
       then value
       else trace "WARNING: Broken 'racket'; falling back to 16.09"
            nixpkgs1609.racket;

  # Racket's 'launcher' system hard-codes PATH to "/bin:/usr/bin" for no good
  # reason, which breaks under Nix. We patch it away.
  patchedRacket = runCommand "patched-racket"
    {
      inherit workingRacket;
      buildInputs = [ replace ];
      file        = "share/racket/collects/launcher/launcher.rkt";
    }
    ''
      cp -r "$workingRacket" "$out"
      chmod +w -R "$out"

      echo "Hard-coding tool checks" 1>&2
      for TOOL in "readlink" "dirname" "basename" "ls" "sed"
      do
        sed -e "s@(has-exe? \"$TOOL\")@#t@g" -i "$out/$file"
      done

      echo "Removing PATH-replacement junk" 1>&2
      sed -e 's@.*"PATH=.*@@g' -i "$out/$file"

      echo "Replacing all references to unpatched Racket '$workingRacket'" 1>&2
      find . -type f | xargs -L 1 replace "$workingRacket" "$out" --
    '';

  racketWithPkgs =
    with rec {
      racketWithDeps = deps: stdenv.mkDerivation {
        name = "racket-with-deps";

        buildInputs = [ makeWrapper patchedRacket ];

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
          for PROG in "${patchedRacket}"/bin/*
          do
            NAME=$(basename "$PROG")
            makeWrapper "$PROG" "$out/bin/$NAME" --set HOME "$out/etc"
          done
        '';
      };
    };
    racketWithDeps [
      # Dependency of grommet
      (fetchFromGitHub {
        owner  = "RayRacine";
        repo   = "grip";
        rev    = "ec498f6";
        sha256 = "06ax30r70sz2hq0dzyassczcdkpmcd4p62zx0jwgc2zp3v0wl89l";
      })

      # Hashing
      (fetchFromGitHub {
        owner  = "RayRacine";
        repo   = "grommet";
        rev    = "50f1b6a";
        sha256 = "1rb7i8jx7gg2rm5flnql0hja4ph11p7i38ryxd04yqw50l0xj59v";
      })

      # Shell commands
      (fetchFromGitHub {
        owner  = "willghatch";
        repo   = "racket-shell-pipeline";
        rev    = "7ed9a75";
        sha256 = "06z5bhmvpdhy4bakh30fzha4s0xp2arjq8h9cyi65b1y18cd148x";
      })
    ];

  tip-repo = fetchFromGitHub {
    owner  = "tip-org";
    repo   = "benchmarks";
    rev    = "fae25da";
    sha256 = "08zm9a8dlwqm6bnd5z8714j5365pklwh4lkgcnhq0ns1lq0njp3l";
  };

  # Generates all the intermediate steps of the transformation
  mkCache = BENCHMARKS: rec {
    inherit BENCHMARKS;

    BENCHMARKS_CACHE = runCommand "benchmarks-cache"
      {
        inherit BENCHMARKS BENCHMARKS_FINAL_BENCHMARK_DEFS
                BENCHMARKS_NORMALISED_DEFINITIONS;
        src         = ./scripts;
        buildInputs = [ env ];
      }
      ''
        "$src/make_sampling_data.rkt" > "$out"
      '';

    BENCHMARKS_NORMALISED_THEOREMS = runCommand "normalised-theorems"
      {
        inherit BENCHMARKS_CACHE BENCHMARKS BENCHMARKS_NORMALISED_DEFINITIONS;
        src         = ./scripts;
        buildInputs = [ env ];
      }
      ''
        "$src/make_normalised_theorems.rkt" > "$out"
      '';

    BENCHMARKS_NORMALISED_DEFINITIONS = runCommand "normalised-definitions"
      {
        inherit BENCHMARKS;
        src         = ./scripts;
        buildInputs = [ env ];
      }
      ''
        "$src/make_normalised_definitions.rkt" > "$out"
      '';

    BENCHMARKS_FINAL_BENCHMARK_DEFS = runCommand "final-defs"
      {
        inherit BENCHMARKS BENCHMARKS_NORMALISED_DEFINITIONS;
        src         = ./scripts;
        buildInputs = [ env ];
      }
      ''
        "$src/gen_final_benchmark_defs.rkt" > "$out"
      '';
  };

  mkTestScript = vars: wrap {
    vars   = {
      TEST_DATA       = ./test-data/nat-simple-raw.json;
      TEST_LIST_EQS   = ./test-data/list-full-found.json;
      TEST_LIST_TRUTH = ./test-data/list-full-ground-truth.smt2;
    } // vars;
    paths  = [ env ];
    script = ''
      #!/usr/bin/env bash
      raco test "${./scripts}/test.rkt" || exit 1
    '';
  };

  runTestScript = given: runCommand "run-test"
    (given // {
      # Allow testing to be skipped, as it can take a few minutes
      doCheck = if getEnv "SKIP_TESTS" == "" then "true" else "false";
    })
    ''
      if $doCheck
      then
        "$script" || exit 1
        echo "passed" > "$out"
      else
        echo "skipped" > "$out"
      fi
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

  # Take benchmarks from git, but transform them to replace "built-in"
  # definitions like "Bool" and "<" with explicitly defined versions.
  tip-benchmarks = stdenv.mkDerivation {
    name = "tip-benchmarks";
    src  = tip-repo;

    buildInputs  = [ env ];
    buildPhase = ''
      set -e
      rm -rf ./transformed
      mkdir -p transformed

      SOURCE="$PWD/benchmarks" \
      DESTINATION="$PWD/transformed" racket "${./scripts/strip-native.rkt}"
    '';

    doCheck = true;
    checkPhase = ''
      set -e
      GIVEN=$(find ./benchmarks  -type f | wc -l)
       MADE=$(find ./transformed -type f | wc -l)

      if [[ "$GIVEN" -ne "$MADE" ]]
      then
        echo "Given $GIVEN benchmarks, outputted $MADE benchmarks" 1>&2
        exit 1
      fi

      # We can't check for '=>' since it's both implication and a function type
      echo "Ensuring there are no native operators..." 1>&2
      while read -r BENCHMARK
      do
        for OP in ite and false not or true True False Bool Int "[+]" "[*]" \
                  div mod ">" "<" ">=" "<="
        do
          # Look for this operator, but avoid matching parts of other symbols
          # (e.g. thinking that "opposite" is "ite") by disallowing characters
          # before/after which are valid in names. Note that we don't check the
          # definition of custom-bool-converter since ite and Bool are
          # unavoidable there.
          if grep -v '(define-fun custom-bool-converter ' < "$BENCHMARK" |
             grep "[^><=a-zA-Z0-9-]$OP[^><=a-zA-Z0-9-]"
          then
            echo "Operator '$OP' should have been replaced in '$BENCHMARK'" 1>&2
            exit 1
          fi
        done

        # Make sure all calls to = and distinct use custom-bool-converter
        for OP in = distinct
        do
          if grep -v "(custom-bool-converter ($OP" < "$BENCHMARK" |
             grep "[^><=a-zA-Z0-9-]$OP[^><=a-zA-Z0-9-]"
          then
            echo "Unguarded '$OP' in '$BENCHMARK'" 1>&2
            exit 1
          fi
        done
      done < <(find ./transformed -type f)

      echo "Checking all benchmarks are parseable by tip" 1>&2
      while read -r BENCHMARK
      do
        OUTPUT=$(tip < "$BENCHMARK") || {
          echo "Error sending $BENCHMARK through tip" 1>&2
          echo "$OUTPUT" 1>&2
          exit 1
        }
      done < <(find ./transformed -type f)
    '';

    installPhase = ''cp -r ./transformed "$out"'';
  };

  # Used for benchmarking the benchmark generation (yo dawg)
  asv = if asv-nix == null
           then nix-config.asv-nix
           else asv-nix;

  # Uses all benchmarks, for our actual results
  cache = mkCache tip-benchmarks;

  testCache =
    with rec {
      testFiles = import ./test-data/test-files.nix;

      testDir = runCommand "test-dir"
        {
          inherit testFiles;
          base = tip-benchmarks;
        }
        ''
          for F in $testFiles
          do
            GO="$out/$F"
            mkdir -p "$(dirname "$GO")"
            ln -s "$base/$F" "$GO"
          done
        '';
    };
    mkCache testDir;

  testScript = mkTestScript testCache;

  # Standalone to allow separate testing and to avoid requiring expensive caches
  quickToolTest = runTestScript { script = testScript; };

  # Standalone since it's too slow to use as a dependency of tools
  fullToolTest = runTestScript {
    inherit quickToolTest;

    script = mkTestScript cache;

    BENCHMARKS_TEST_ALL = "1";

    # Check contracts while testing; it's disabled by default for being too slow
    PLT_TR_CONTRACTS    = "1";
  };

  # Installs tools for translating, sampling, etc. the benchmark. These tools
  # get cached data baked into them, which makes them slow to install but fast
  # to run.
  tools = stdenv.mkDerivation (cache // rec {
    # Require (quick) tests to pass before attempting to install
    inherit quickToolTest;

    name = "te-benchmark";
    src  = ./scripts;

    buildInputs = [ env makeWrapper ];

    installPhase = ''
      # Install Racket scripts
      mkdir "$out"
      cp -r . "$out/scripts"

      # Compile Racket to bytecode for speed
      raco make "$out/scripts/"*.rkt "$out/scripts/lib/"*.rkt

      # For each script, add a wrapper to PATH, without the .rkt suffix
      mkdir -p "$out/bin"
      for F in "$out/scripts/"*.rkt
      do
        NAME=$(basename "$F" .rkt)

        # Write a one-liner to invoke this script, since shebangs don't seem to
        # use the bytecode
        printf '#!/usr/bin/env bash\nexec racket "%s" "$@"' \
               "$F" > "$out/bin/$NAME"
        chmod +x "$out/bin/$NAME"

        # Wrap the one-liner so we can provide an appropriate environment.
        # Set PLT_COMPILED_FILE_CHECK to avoid checking bytecode timestamps.
        wrapProgram "$out/bin/$NAME"                                             \
          --prefix PATH : "${env}/bin"                                           \
          --set PWD                            "$out/scripts"                    \
          --set PLT_COMPILED_FILE_CHECK        exists                            \
          --set BENCHMARKS_CACHE               "$BENCHMARKS_CACHE"               \
          --set BENCHMARKS_NORMALISED_THEOREMS "$BENCHMARKS_NORMALISED_THEOREMS" \
          --set BENCHMARKS                     "$BENCHMARKS"
      done
    '';
  });

  compileRacketScript = name: vars: script: wrap {
    inherit name;
    paths = [ racketWithPkgs ];
    vars  = {
      inherit PLTCOLLECTS;
      PLT_COMPILED_FILE_CHECK = "exists";
    } // vars;
    file = runCommand "compiled-${name}"
      {
        inherit PLTCOLLECTS;
        buildInputs = [ racketWithPkgs ];
        fileName    = name;
        raw         = if typeOf script == "path" || hasPrefix "/" script
                         then script
                         else writeScript "${name}.rkt" ''
                           #!/usr/bin/env racket
                           #lang racket
                           ${script}
                         '';
      }
      ''
        echo "Compiling '$raw' to '$out'" 1>&2

        # The '--gui' flag somehow works around this problem with 'raco exe'
        # https://github.com/NixOS/nixpkgs/issues/11698
        # Apparently newer Racket releases don't have this problem, but we need
        # x86 compatibility which the newer releases drop (in Nixpkgs, at least)
        raco exe --gui -o "$out" "$raw"
      '';
  };

  compileTest = runCommand "compile-test"
    {
      buildInputs = [ fail ];
      cmd = compileRacketScript "test-script" {} ''
        (display "Compiled Racket scripts work")
      '';
    }
    ''
      set -e
      OUTPUT=$("$cmd") || fail "Compiled script failed"
      echo "$OUTPUT" | grep 'work' || fail "Didn't output correctly"
      mkdir "$out"
    '';

  runRacket = name: paths: vars: script: stdenv.mkDerivation {
    inherit name;
    builder = wrap {
      name   = "${name}.rkt";
      paths  = [ racketWithPkgs ] ++ paths;
      vars   = { PLTCOLLECTS = ":${./scripts}"; } // vars;
      script = ''
        #!/usr/bin/env racket
        #lang racket
        ${script}
      '';
    };
  };

  tip-benchmark-smtlib = runRacket "tip-benchmark-smtlib"
    []
    { inherit (cache) BENCHMARKS BENCHMARKS_FINAL_BENCHMARK_DEFS; }
    ''
      (require lib/normalise)
      (require lib/impure)

      (write-to-out (mk-final-defs))
    '';

  tip-benchmark-haskell = stdenv.mkDerivation {
    inherit (cache) BENCHMARKS_FINAL_BENCHMARK_DEFS;
    name         = "tip-benchmark-haskell";
    buildInputs  = [ tools ];
    buildCommand = ''
      mkdir "$out"
      OUT_DIR="$out" tip_haskell_package
    '';
  };
}
