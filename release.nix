{ supportedSystems ? [ "i686-linux" "x86_64-linux" ] }:

with builtins;
with (import <nixpkgs> {}).lib;

genAttrs
  supportedSystems
  (system:
    with import ./pkgs.nix { pkgsArgs = { inherit system; }; };
    with {
      # Select the derivations we care about
      parts = haskellPackages:
        with callPackage ./. {
          inherit haskellPackages;
          pkgsArgs = { inherit system; };
        };
        {
          inherit asv tip-benchmark-haskell tip-benchmark-smtlib tools;
          inherit (patchedHaskellPackages) tip-lib;
          quickTests = tests { full = false; };
          fullTests  = tests { full = true;  };
        };
    };
    # Add more versions to this list if we can get them to work
    mapAttrs (_: parts)
             (filterAttrs (n: _: elem n [ "ghc7103" "ghc801" "ghc802" ])
                          haskell.packages))
