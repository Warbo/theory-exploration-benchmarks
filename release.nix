rec {

  te-benchmark   = builtins.trace "FIXME: Test against all systems and Haskell versions"
                     (import ./shell.nix);

  tip-benchmarks = (import <nixpkgs> {}).stdenv.mkDerivation {
                     name = "tip-benchmarks";
                     buildInputs = [ te-benchmark ];
                     buildCommand = ''
                       source $stdenv/setup
                       set -e

                       # fullTePkg expects OUT_DIR env var
                       OUT_DIR="$PWD/tip-benchmark-sig"
                       mkdir -p "$OUT_DIR"
                       export OUT_DIR

                       # Create Haskell package
                       fullTePkg

                       # Store Haskell package
                       cp -a "$OUT_DIR" "$out"
                     '';
                   };
}
