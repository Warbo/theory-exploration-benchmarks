{ nix-config-src ? null, pkgsArgs ? { config = {}; }, pkgsPath ? null }:

with rec {
  # Known-good version of nixpkgs
  repo1609 = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "f22817d";
    sha256 = "1cx5cfsp4iiwq8921c15chn1mhjgzydvhdcmrvjmqzinxyz71bzh";
  };

  nixpkgs1609 = import repo1609 pkgsArgs;

  pkgsWithArgs = import (if pkgsPath == null
                            then repo1609
                            else pkgsPath);

  pkgs = pkgsWithArgs pkgsArgs;

  # Custom packages, overrides, etc.

  nix-config-src-default = pkgs.fetchgit {
    url    = "http://chriswarbo.net/git/nix-config.git";
    rev    = "15e860d";
    sha256 = "18x4cq2cl8dmw8zkk4a4kryh53bj98n61ydj1472ywhmvkanw944";
  };

  config-src = if nix-config-src == null
                  then nix-config-src-default
                  else nix-config-src;

  nix-config = pkgsWithArgs (pkgsArgs // {
                              config = import "${config-src}/config.nix";
                            });
};

# Expose the contents of pkgs, along with some other useful definitions
pkgs // builtins // pkgs.lib // {
  inherit nixpkgs1609 nix-config;
  inherit (nix-config) attrsToDirs callPackage fail nothing replace withDeps
                       wrap;
}
