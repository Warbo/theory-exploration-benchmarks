# Builds an environment in which to run benchmarks
{
  dir,  # The commit we're using to find our benchmarks
  root  # The commit we're benchmarking
}:
with import dir {};
with nix-config;
with rec {
  inherit (testCache)
    BENCHMARKS_CACHE
    BENCHMARKS_FALLBACK
    BENCHMARKS_FINAL_BENCHMARK_DEFS
    BENCHMARKS_NORMALISED_DEFINITIONS
    BENCHMARKS_NORMALISED_THEOREMS
    TEST_DATA;

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

  deps = attrsToDirs {
    bin = {
      run_tests   = profileWith testCache "test.sh";

      mk_defs     = profileWith { inherit BENCHMARKS_FALLBACK; }
                                "make_normalised_definitions.rkt";

      mk_thms     = profileWith { inherit BENCHMARKS_CACHE
                                          BENCHMARKS_FALLBACK
                                          BENCHMARKS_NORMALISED_DEFINITIONS; }
                                "make_normalised_theorems.rkt";

      mk_sdata    = profileWith { inherit BENCHMARKS_FALLBACK
                                          BENCHMARKS_FINAL_BENCHMARK_DEFS
                                          BENCHMARKS_NORMALISED_DEFINITIONS; }
                                "make_sampling_data.rkt";

      mk_fin_defs = profileWith { inherit BENCHMARKS_FALLBACK
                                          BENCHMARKS_NORMALISED_DEFINITIONS; }
                                "gen_final_benchmark_defs.rkt";
    };
  };
};
attrsToDirs {
  bin = {
    python = wrap {
      paths  = [ deps ];
      script = runner "${python}/bin/python";
    };
  };
}
