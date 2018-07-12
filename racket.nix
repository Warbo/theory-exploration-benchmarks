{ fail, fetchFromGitHub, fetchgit, lib, makeWrapper, nixListToBashArray,
  nixpkgs1609, pkgs, runCommand, stdenv, writeScript, withDeps, wrap }:

with builtins;
with lib;
rec {
  # Racket may be disabled ( https://github.com/NixOS/nixpkgs/pull/23542 )
  workingRacket =
    with (tryEval pkgs.racket);
    if success
       then value
       else trace "WARNING: Broken 'racket'; falling back to 16.09"
            nixpkgs1609.racket;

  # Wrap each Racket binary in an env where PLTCOLLECTS is set
  racketWithPkgs = PLTCOLLECTS: runCommand "racket-with-deps"
    {
      inherit PLTCOLLECTS;
      base        = racketWithPkgsBase;
      buildInputs = [ makeWrapper ];
    }
    ''
      mkdir -p "$out/bin"
      for PROG in "$base"/bin/*
      do
        NAME=$(basename "$PROG")
        makeWrapper "$PROG" "$out/bin/$NAME"     \
                    --set HOME        "$out/etc" \
                    --set PLTCOLLECTS "$PLTCOLLECTS"
      done
    '';

  racketWithPkgsBase =
    with rec {
      racketWithDeps = deps: stdenv.mkDerivation {
        name = "racket-with-deps-base";

        buildInputs = [ makeWrapper workingRacket ];

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
          for PROG in "${workingRacket}"/bin/*
          do
            NAME=$(basename "$PROG")
            makeWrapper "$PROG" "$out/bin/$NAME" --set HOME "$out/etc"
          done
        '';
      };
    };
    racketWithDeps [
      # Dependency of grommet
      (fetchgit {
        url    = https://gitlab.com/RayRacine/grip.git;
        rev    = "ec498f6";
        sha256 = "06ax30r70sz2hq0dzyassczcdkpmcd4p62zx0jwgc2zp3v0wl89l";
      })

      # Hashing
      (fetchgit {
        url    = https://gitlab.com/RayRacine/grommet.git;
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

  # Put the given scripts in a directory and make them available to Racket. This
  # is useful for depending on a sub-set of scripts, to prevent rebuilding all
  # of the early stages when only the later stages have been edited.
  mkPLTCOLLECTS = given:
    assert isList given;
    assert all isString given;
    with rec {
      scripts = racketScriptDeps given;

      # Take care to make paths which point to the original files, so that only
      # those files get added to the store. We want to avoid getting the whole
      # ./scripts directory added to the store, since that would affect our
      # hashes and cause rebuilds (the one thing we did not want to happen).
      mkPath = f: ./scripts + "/${f}";

      srcs   = map mkPath scripts;

      sDests = nixListToBashArray { name = "SCRIPTDESTS"; args = scripts; };

      sSrcs  = nixListToBashArray { name = "SCRIPTSRCS";  args = srcs;    };

      dir    = runCommand "pltcollects-dir"
        (sDests.env // sSrcs.env)
        ''
          ${sDests.code}
          ${ sSrcs.code}

          # Loop over each src/dest pair in our arrays (off-by-one cruft is due
          # to seq preferring to count from 1)
          mkdir -p "$out"
          for NPLUSONE in $(seq 1 "''${#SCRIPTSRCS[@]}")
          do
            N=$(( NPLUSONE - 1 ))

             SRC="''${SCRIPTSRCS[$N]}"
            DEST="$out/''${SCRIPTDESTS[$N]}"

            DEST_DIR=$(dirname "$DEST")
            mkdir -p "$DEST_DIR"

            ln -v -s "$SRC" "$DEST"
          done
        '';
    };
    ":${dir}";

  # Contains all of our scripts for Racket to import
  PLTCOLLECTS = ":${./scripts}";

  compileRacketScript =
    with rec {
      go = name: vars: paths: script: wrap {
        inherit name;
        paths = [ (racketWithPkgs PLTCOLLECTS) ] ++ paths;
        vars  = {
          inherit PLTCOLLECTS;
          PLT_COMPILED_FILE_CHECK = "exists";
        } // vars;
        file = runCommand "compiled-${name}"
          ({
            inherit PLTCOLLECTS;
            buildInputs = [ (racketWithPkgs PLTCOLLECTS) ];
            fileName    = name;
            raw         = if typeOf script == "path" || hasPrefix "/" script
                             then script
                             else writeScript "${name}.rkt" ''
                               #!/usr/bin/env racket
                               #lang racket
                               ${script}
                             '';
          } // vars)  # Adding vars here lets us run tests before compiling
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
          cmd = go "rkt-test" {} []
                   ''(display "Compiled Racket scripts work")'';
        }
      ''
        set -e
        OUTPUT=$("$cmd") || fail "Compiled script failed"
        echo "$OUTPUT" | grep 'work' || fail "Didn't output correctly"
        mkdir "$out"
      '';
    };
    name: vars: paths: script: withDeps [ compileTest ]
                                        (go name vars paths script);

  # Like 'runCommand', but uses Racket code for the builder instead of bash
  runRacket = name: env: vars: script:
    with rec {
      deps     = libDepsOfString script;
      depCheck = vars.depCheck or (deps: true);
      extra    = assert depCheck deps; { PLTCOLLECTS = mkPLTCOLLECTS deps; };
    };
    stdenv.mkDerivation {
      inherit name;
      builder = wrap {
        name   = "${name}.rkt";
        paths  = [ racketWithPkgsBase (env extra) ];
        vars   = removeAttrs (vars // extra) [ "depCheck" ];
        script = ''
          #!/usr/bin/env racket
          #lang racket
          ${script}
        '';
      };
    };

  # Find deps between scripts/lib files by looking at their 'require' statements
  libDepsOfString =
    with rec {
      trimFromLeft = pre:  s: concatStringsSep pre (tail (splitString pre s));

      takeUntil    = post: s: head (splitString post s);

      partsAfter   = pre:  s: tail (splitString pre s);
    };
    s: racketScriptDeps (map (x: "lib/" + (takeUntil ")" x) + ".rkt")
                             (partsAfter "(require lib/" s));

  libDepsOfFile = f: import (runCommand "racket-deps-of-${baseNameOf f}"
    { f = ./scripts + "/${f}"; }
    ''
      echo '[' > "$out"
        while read -r DEP
        do
          echo "\"lib/$DEP.rkt\""
        done < <(grep '(require lib/' < "$f" |
                 sed -e 's@(require lib/@@g' |
                 tr -d ' ()') >> "$out"
      echo ']' >> "$out"
    '');

  racketScriptDeps =
    with rec {
      step    = f: [f] ++ libDepsOfFile f;

      allDeps = known: with { next = collapse (concatMap step known); };
                       if known == next
                          then known
                          else allDeps next;

      collapse = l: unique (sort (x: y: x < y) l);
    };
    given: with { result = allDeps given; };
           assert all (x: elem x result) given;
           result;
}
