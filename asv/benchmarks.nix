# Builds an environment in which to run benchmarks
{
  dir,  # The commit we're using to find our benchmarks
  root  # The commit we're benchmarking
}:
with import dir {};
with nix-config;
with rec {
  # Uses a small selection of benchmarks, useful for profiling, etc.
  fewCache = mkCache (runCommand "few" { ALL = tip-benchmarks; } ''
    pushd "$ALL"
    while read -r F
    do
      DIR=$(dirname "$out/$F")
      mkdir -p "$DIR"
      cp -v "$F" "$DIR"/
    done < <(find . -name "*.smt2" | sort | head -n20)
  '');

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
      run_tests = profileWith { inherit (cache) BENCHMARKS_FALLBACK TEST_DATA; }
                              "test.sh";

      mk_defs   = profileWith { inherit (testCache) BENCHMARKS_FALLBACK; }
                              "make_normalised_definitions.rkt";

      mk_thms   = profileWith { inherit (testCache)
                                  BENCHMARKS_FALLBACK
                                  BENCHMARKS_NORMALISED_DEFINITIONS; }
                              "make_normalised_theorems.rkt";

      mk_sdata  = profileWith { inherit (testCache)
                                  BENCHMARKS_FALLBACK
                                  BENCHMARKS_NORMALISED_DEFINITIONS; }
                              "make_sampling_data.rkt";
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
