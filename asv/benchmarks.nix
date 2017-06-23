# Builds an environment in which to run benchmarks
{
  dir,  # The commit we're using to find our benchmarks
  root  # The commit we're benchmarking
}:
with import dir {};
with nix-config;
with {
  # Take these from root so we can measure performance across revisions
  scripts = root + "/scripts";

  deps = attrsToDirs {
    bin = {
      run_tests = wrap {
        vars   = { inherit (cache) BENCHMARKS_FALLBACK TEST_DATA; };
        paths  = [ env ];
        script = ''
          #!/usr/bin/env bash
          exec "${scripts}/test.sh"
        '';
      };

      mk_defs = wrap {
        vars   = { BENCHMARKS_FALLBACK = cache.BENCHMARKS_FEW; };
        paths  = [ env ];
        script = ''
          #!/usr/bin/env bash
          exec "${scripts}/make_normalised_definitions.rkt" > /dev/null
        '';
      };
    };
  };
};
attrsToDirs {
  bin = {
    python = wrap {
      paths  = [ deps ];
      script = ''
        #!/usr/bin/env bash
        exec "${python}/bin/python"
      '';
    };
  };
}
