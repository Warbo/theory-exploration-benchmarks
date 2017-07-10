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

  deps = attrsToDirs {
    bin = {
      run_tests   = testScript;

      mk_defs     = profileWith { inherit BENCHMARKS; }
                                "make_normalised_definitions.rkt";

      more_defs   = profileWith { BENCHMARKS = runCommand "larger-test"
                                    { inherit (cache) BENCHMARKS; }
                                    ''
                                      find "$BENCHMARKS" -type f -o -type l |
                                      head -n100                            |
                                      while read -r F
                                      do
                                        DIR=$(basename "$(dirname "$F")")
                                        mkdir -p "$out/$DIR"
                                        cp -s "$F" "$out/$DIR"/
                                      done
                                    ''; }
                                "make_normalised_definitions.rkt";

      mk_thms     = profileWith { inherit BENCHMARKS_CACHE
                                          BENCHMARKS
                                          BENCHMARKS_NORMALISED_DEFINITIONS; }
                                "make_normalised_theorems.rkt";

      mk_sdata    = profileWith { inherit BENCHMARKS
                                          BENCHMARKS_FINAL_BENCHMARK_DEFS
                                          BENCHMARKS_NORMALISED_DEFINITIONS; }
                                "make_sampling_data.rkt";

      mk_fin_defs = profileWith { inherit BENCHMARKS
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
