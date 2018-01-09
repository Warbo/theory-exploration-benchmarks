# Tons of Inductive Problems: benchmarks, tools, dependencies, etc.
{ bash, buildEnv, fetchFromGitHub, haskellPackages, lib, nix-config, pkgs,
  racketWithPkgs }:

with lib;
rec {
  # Take the given Haskell packages, but override some things which are known
  # to be broken on Hackage. TODO: Get upstream to upload non-broken packages!
  patchedHaskellPackages =
    with rec {
      hsPkgs = if haskellPackages == null
                  then pkgs.haskell.packages.ghc7103
                  else haskellPackages;

      overriddenHaskell = name:
        nix-config.haskell.packages.ghc7103."${name}".src;
    };
    hsPkgs.override {
      overrides = self: super:
        genAttrs [ "tip-lib" "geniplate" ]
                 (name: self.callPackage (overriddenHaskell name) {});
    };

  tip-repo = fetchFromGitHub {
    owner  = "tip-org";
    repo   = "benchmarks";
    rev    = "fae25da";
    sha256 = "08zm9a8dlwqm6bnd5z8714j5365pklwh4lkgcnhq0ns1lq0njp3l";
  };

  env = buildEnv {
    name  = "tip-bench-env";
    paths = [
      bash
      patchedHaskellPackages.cabal-install
      racketWithPkgs
      (patchedHaskellPackages.ghcWithPackages (hs: [
        hs.tip-lib
        hs.geniplate
        hs.QuickCheck
        hs.quickspec
        hs.testing-feat
        hs.cereal
        hs.murmur-hash
      ]))
    ];
  };
}
