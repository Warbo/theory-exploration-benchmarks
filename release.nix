with import <nixpkgs> {};
with lib;
with builtins;

# Ignore some Haskell versions, to save memory
let discard    = v: !(elem v [ "ghc7103" ]);

    hsVersions = filterAttrs (n: _: !(discard n)) haskell.packages;

 in lib.mapAttrs (n: v: import ./. {
                          inherit bash racket stdenv writeScript;
                          haskellPackages = v;
                        })
                 hsVersions
