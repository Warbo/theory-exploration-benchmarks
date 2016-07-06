{
  te-benchmark = builtins.trace "FIXME: Test against all systems and Haskell versions"
                   (import ./shell.nix);
}
