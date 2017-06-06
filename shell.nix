with import ./. {};
(import <nixpkgs> {}).stdenv.mkDerivation {
  name         = "te-benchmark-env";
  buildInputs  = [ env ];
  buildCommand = "exit 1";
  shellHook    = ''
    {
      echo "Setting BENCHMARKS_FALLBACK to ${tip-benchmarks}"
      echo "To use a different set of benchmarks, you can set BENCHMARKS"
      export BENCHMARKS_FALLBACK="${tip-benchmarks}"

      echo "Setting TEST_DATA to ${./test-data}"
      export TEST_DATA="${./test-data}"

      echo "NOTE: We don't check Racket contracts because it's slow."
      echo "To enable contract checking, set PLT_TR_CONTRACTS to 1"

      echo "You can run tests with e.g. 'raco test scripts/test.rkt'"
      echo "Use PLT_TEST_REGEX env var to limit which test cases are run."

      echo "Log messages are suppressed during tests. Set DEBUG to see them."
    } 1>&2
  '';
}
