# Scripts to benchmark. These are invoked by benchmarks.py, but defined here to
# make dependencies, environments, etc. easier to define.

with import ./.. {};
with nix-config;
attrsToDirs {
  bin = {
    run_tests = wrap {
      vars   = { inherit (cache) BENCHMARKS_FALLBACK TEST_DATA; };
      paths  = [ env ];
      script = ''
        #!/usr/bin/env bash
        exec "${../scripts}/test.sh"
      '';
    };

    mk_defs = wrap {
      vars   = { BENCHMARKS_FALLBACK = cache.BENCHMARKS_FEW; };
      paths  = [ env ];
      script = ''
        #!/usr/bin/env bash
        exec "${../scripts}/make_normalised_definitions.rkt" > /dev/null
      '';
    };
  };
}
