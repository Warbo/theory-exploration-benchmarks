with builtins;
with (import <nixpkgs> {}).lib;
with import ./pkgs.nix {};
with {
  # Select the derivations we care about
  parts = haskellPackages:
    with callPackage ./. { inherit haskellPackages; };
    {
      # Useful infrastructure
      inherit asv env scripts tools;
      inherit (patchedHaskellPackages) tip-lib;

      # Benchmark data
      inherit tip-benchmark-haskell tip-benchmark-smtlib;

      # Tests
      quickTests = tests { full = false; };
      fullTests  = tests { full = true;  };
    };
};
# Add more versions to this list if we can get them to work
mapAttrs (_: parts)
         (filterAttrs (n: _: elem n [ "ghc7103" "ghc801" "ghc802" ])
                      haskell.packages)
