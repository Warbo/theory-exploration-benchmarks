{ supportedSystems ? [ "i686-linux" "x86_64-linux" ] }:

with builtins;

let forSystem = system:
  with import <nixpkgs> { inherit system; };
  with lib;

  # Ignore some Haskell versions, to save memory
  let discard    = v: !(elem v [ "ghc7103" "ghc801" ]);

      hsVersions = filterAttrs (n: _: !(discard n)) haskell.packages;

   in lib.mapAttrs (n: v: import ./. {
                            inherit bash fetchurl python racket stdenv
                                    writeScript;
                            haskellPackages = v;
                          })
                   hsVersions;
 in listToAttrs (map (system: { name = system; value = forSystem system; })
                     supportedSystems)
