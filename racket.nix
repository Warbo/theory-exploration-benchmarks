{ fail, fetchFromGitHub, lib, makeWrapper, nixpkgs1609, pkgs, runCommand,
  stdenv, writeScript, withDeps, wrap }:

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

  racketWithPkgs =
    with rec {
      racketWithDeps = deps: stdenv.mkDerivation {
        name = "racket-with-deps";

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

          # Provide Racket binaries patched to use our modified HOME and scripts
          mkdir -p "$out/bin"
          for PROG in "${workingRacket}"/bin/*
          do
            NAME=$(basename "$PROG")
            makeWrapper "$PROG" "$out/bin/$NAME"     \
                        --set HOME        "$out/etc" \
                        --set PLTCOLLECTS "${PLTCOLLECTS}"
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

  # Env var so Racket can 'require' our scripts
  PLTCOLLECTS = ":${./scripts}";

  compileRacketScript =
    with rec {
      go = name: vars: script: wrap {
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
          cmd = go "rkt-test" {} ''(display "Compiled Racket scripts work")'';
        }
      ''
        set -e
        OUTPUT=$("$cmd") || fail "Compiled script failed"
        echo "$OUTPUT" | grep 'work' || fail "Didn't output correctly"
        mkdir "$out"
      '';
    };
    name: vars: script: withDeps [ compileTest ] (go name vars script);

  # Like 'runCommand', but uses Racket code for the builder instead of bash
  runRacket = name: paths: vars: script: stdenv.mkDerivation {
    inherit name;
    builder = wrap {
      name   = "${name}.rkt";
      paths  = [ racketWithPkgs ] ++ paths;
      vars   = { inherit PLTCOLLECTS; } // vars;
      script = ''
        #!/usr/bin/env racket
        #lang racket
        ${script}
      '';
    };
  };
}
