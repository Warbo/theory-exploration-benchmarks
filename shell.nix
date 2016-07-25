with import <nixpkgs> {};

let all = callPackage ./. { haskellPackages = haskell.packages.ghc7103; };
 in all.te-benchmark
