{ supportedSystems ? [ "i686-linux" "x86_64-linux" ] }:

with builtins;
with (import <nixpkgs> {}).lib;
with rec {
  forSys = system:
    with rec {
      pkgs = import <nixpkgs> { inherit system; };

      # Add more versions to this list if we can get them to work
      hsVersions = filterAttrs (n: _: elem n [ "ghc7103" "ghc801" "ghc802" ])
                               pkgs.haskell.packages;

      # Remove attributes added by 'callPackage'. Also avoid building the whole
      # of 'patchedHaskellPackages', when we only care about tip-lib.
      strip = x: removeAttrs x [ "override" "overrideDerivation" "nix-config"
                                 "patchedHaskellPackages" ] //
                 { inherit (x.patchedHaskellPackages) tip-lib; };
   };
   mapAttrs (_: haskellPackages: strip (pkgs.callPackage ./. {
                                         inherit haskellPackages;
                                         pkgsArgs = { inherit system; };
                                       }))
            hsVersions;
};
genAttrs supportedSystems forSys
