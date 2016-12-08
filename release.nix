{ supportedSystems ? [ "i686-linux" "x86_64-linux" ] }:

with builtins;

let forPkgs = pkgs:
  with pkgs;
  with lib;

  # Ignore most Haskell versions, to save memory
  let hsVersions = filterAttrs (n: _: elem n [ "ghc7103" "ghc801" ])
                               haskell.packages;

      # Remove attributes added by 'callPackage'
      strip = filterAttrs (n: v: !(elem n [
                            "override" "overrideDerivation"
                          ]));

   in mapAttrs (n: v: strip (callPackage ./. {
                              inherit pkgs;
                              haskellPackages = v;
                            }))
               hsVersions;

 in listToAttrs (map (system: {
                       name  = system;
                       value = forPkgs (import <nixpkgs> { inherit system; });
                     })
                     supportedSystems)
