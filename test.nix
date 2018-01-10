{ cache, env, fail, lib, nothing, runCommand, tip-repo, withDeps, writeScript }:

with builtins;
with lib;
rec {
  runTest = { full ? false, file }: runCommand "run-tests"
    (cache // {
      inherit file;

      # Check contracts and use all files when testing; slow, so off by default.
      PLT_TR_CONTRACTS  = if full then "1" else null;

      # Data used by some of our tests
      TEST_DATA         = ./test-data/nat-simple-raw.json;
      TEST_LIST_EQS     = ./test-data/list-full-found.json;
      TEST_LIST_TRUTH   = ./test-data/list-full-ground-truth.smt2;
      BENCHMARKS_SOURCE = tip-repo + "/benchmarks";

      buildInputs       = [ env fail ];
    })
    ''
      set -e
      raco test "$file" || fail "Tests failed"
      mkdir "$out"
    '';

  tests = { full ? false }: genAttrs (map (removeSuffix ".rkt")
                                          (attrNames (readDir ./scripts/lib)))
                                     (f: runTest {
                                       inherit full;
                                       file = ./scripts/lib + "/${f}.rkt";
                                     });
}
