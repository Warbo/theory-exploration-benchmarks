with import <nixpkgs> {};

callPackage ./. { haskellPackages = haskell.packages.ghc7103; }