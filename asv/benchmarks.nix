# Builds an environment in which to run benchmarks
{
  dir,  # The commit we're using to find our benchmarks
  root  # The commit we're benchmarking
}:
with import dir {};
with nix-config;
with rec {
  inherit (testCache)
    BENCHMARKS
    BENCHMARKS_CACHE
    BENCHMARKS_FINAL_BENCHMARK_DEFS
    BENCHMARKS_NORMALISED_DEFINITIONS
    BENCHMARKS_NORMALISED_THEOREMS;

  # Take these from root so we can measure performance across revisions
  scripts = root + "/scripts";

  runner = cmd: ''
    #!/usr/bin/env bash
    exec "${cmd}" "$@"
  '';

  profileWith = vars: script: wrap {
    inherit vars;
    paths = [ env ];
    script = runner "${scripts}/${script}";
  };

  deps = attrsToDirs { bin = { run_tests = testScript; }; };
};
attrsToDirs {
  bin = {
    python = wrap {
      paths  = [ deps ];
      script = runner "${python}/bin/python";
    };
  };
}
