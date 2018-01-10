with import ./. {};
(import <nixpkgs> {}).stdenv.mkDerivation {
  name         = "te-benchmark-env";
  buildInputs  = [ env ];
  buildCommand = "exit 1";
  shellHook    = ''
    {
      export BENCHMARKS="${tip-benchmarks}"
      echo "Set BENCHMARKS to $BENCHMARKS"

      export TEST_DATA="${./test-data/nat-simple-raw.json}"
      echo "Set TEST_DATA to $TEST_DATA"

      echo "NOTE: We don't check Racket contracts because it's slow."
      echo "To enable contract checking, set PLT_TR_CONTRACTS to 1"

      echo "You can run tests with e.g. 'raco test scripts/lib/foo.rkt'"
      echo "Use PLT_TEST_REGEX env var to limit which test cases are run."

      echo "Log messages are suppressed during tests. Set DEBUG to see them."
    } 1>&2
  '';
}
