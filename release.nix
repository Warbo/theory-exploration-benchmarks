with import <nixpkgs> {};

lib.mapAttrs (n: v: import ./. {
                      inherit bash racket stdenv writeScript;
                      haskellPackages = v;
                    })
             haskell.packages
