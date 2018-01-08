{ env, runRacket, tip-repo }:

# Generates all the intermediate steps of the transformation, so that scripts
# can mostly just perform look-ups.
{
  # Use a set in a set, to avoid cruft like 'override' getting included.
  cache = rec {
    # Take benchmarks from git, but transform them to replace "built-in"
    # definitions like "Bool" and "<" with explicitly defined versions.
    BENCHMARKS = runRacket "tip-benchmarks" [ env ] { repo = tip-repo; } ''
      (require lib/strip-native)

      (define source
        (mk-source (getenv "repo")))

      (define destination
        (getenv "out"))

      (define input-files
        (tip-files-in source))

      ;; Generate actual output
      (for-each (process-tip-file! source destination)
                input-files)
    '';

    BENCHMARKS_CACHE = runRacket "benchmarks-cache" [ env ]
      {
        inherit BENCHMARKS BENCHMARKS_FINAL_BENCHMARK_DEFS
                BENCHMARKS_NORMALISED_DEFINITIONS;
      }
      ''
        (require lib/impure)
        (require lib/sampling)

        ;; Generates data about renaming, etc. which can be cached and re-used
        ;; to make sampling and querying quicker.
        (write-to-out (format "~s" (make-sampling-data)))
      '';

    BENCHMARKS_NORMALISED_THEOREMS = runRacket "normalised-theorems" [ env ]
      { inherit BENCHMARKS_CACHE BENCHMARKS BENCHMARKS_NORMALISED_DEFINITIONS; }
      ''
        ;; Replaced (assoc-get 'normalised-theorems (get-sampling-data))
        ;; Write normalised-theorems return value to BENCHMARKS_NORMALISED_THEOREMS
        (require lib/impure)
        (require lib/theorems)
        (write-to-out (format "~s" (normalised-theorems)))
      '';

    BENCHMARKS_NORMALISED_DEFINITIONS = runRacket "normalised-definitions"
      [ env ]
      { inherit BENCHMARKS; }
      ''
        (require lib/impure)
        (require lib/normalise)
        (write-to-out
          (format "~s"
            (gen-normed-and-replacements)))
      '';

    BENCHMARKS_FINAL_BENCHMARK_DEFS = runRacket "final-defs"
      [ env ]
      { inherit BENCHMARKS BENCHMARKS_NORMALISED_DEFINITIONS; }
      ''
        (require lib/impure)
        (require lib/normalise)
        (require lib/tip)
        (write-to-out
          (format "~s"
            (prepare (first (normed-and-replacements-cached)))))
      '';
  };
}
