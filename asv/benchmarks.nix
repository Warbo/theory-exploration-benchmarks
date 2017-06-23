# Builds an environment in which to run benchmarks
{
  dir,  # The commit we're using to find our benchmarks
  root  # The commit we're benchmarking
}:
with import dir {};
with nix-config;
with rec {
  # Take these from root so we can measure performance across revisions
  scripts = root + "/scripts";

  runner = cmd: ''
    #!/usr/bin/env bash
    exec "${cmd}" "$@"
  '';

  deps = attrsToDirs {
    bin = {
      run_tests = wrap {
        vars   = { inherit (cache) BENCHMARKS_FALLBACK TEST_DATA; };
        paths  = [ env ];
        script = runner "${scripts}/test.sh";
      };

      mk_defs = wrap {
        vars   = { BENCHMARKS_FALLBACK = cache.BENCHMARKS_FEW; };
        paths  = [ env ];
        script = runner "${scripts}/make_normalised_definitions.rkt";
      };
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
