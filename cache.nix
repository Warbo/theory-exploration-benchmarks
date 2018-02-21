{ env, runRacket, tip-repo }:

# Generates all the intermediate steps of the transformation, so that scripts
# can mostly just perform look-ups.
{
  # Use a set in a set, to avoid cruft like 'override' getting included.
  cache = rec {
    # Take benchmarks from git, but transform them to replace "built-in"
    # definitions like "Bool" and "<" with explicitly defined versions.
    BENCHMARKS = runRacket "tip-benchmarks" env { repo = tip-repo; } ''
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

    BENCHMARKS_THEOREM_DEPS = runRacket "benchmarks-cache" env
      {
        inherit BENCHMARKS BENCHMARKS_FINAL_BENCHMARK_DEFS
                BENCHMARKS_NORMALISED_DEFINITIONS
                BENCHMARKS_NORMALISED_THEOREMS;
      }
      ''
        (require lib/conjectures)
        (require lib/impure)
        (require lib/normalise)
        (require lib/sampling)
        (require lib/theorems)

        ;; read/write doesn't work for sets, so convert to list
        (write-to-out
          (format "~s" (map (lambda (t-d)
                              (list (first t-d) (set->list (second t-d))))
                            (all-theorem-deps))))
      '';

    BENCHMARKS_NORMALISED_THEOREMS = runRacket "normalised-theorems" env
      { inherit BENCHMARKS BENCHMARKS_NORMALISED_DEFINITIONS; }
      ''
        (require lib/impure)
        (require lib/lists)
        (require lib/normalise)
        (require lib/replacements)
        (require lib/theorems)
        (require lib/util)

        ;; Extend function replacements with replacement of constructors by
        ;; constructor functions

        (define constructor-replacements
          (constructor-function-replacements (first (qual-hashes-theorem-files))
                                             (all-replacements-closure)))

        (define final-replacements
          (finalise-replacements
            (extend-replacements (all-replacements-closure)
                                 constructor-replacements)))

        (define (prefix-custom-constructors x)
          (replace-all (map (lambda (name)
                              (list name (prefix-name name "constructor-")))
                            '(CustomTrue CustomFalse CustomZ CustomS CustomNeg
                              CustomZero CustomPos))
                       x))

         (define (prefix-custom-destructors x)
           (replace-all (map (lambda (name)
                               (list name (prefix-name name "destructor-")))
                             '(custom-p custom-pred custom-succ))
                        x))

        ;; This should have been unwrapped already
        (define (assert-no-convert x)
          (match x
            ['custom-bool-converter (error "Lingering custom-bool-converter")]
            [(cons a b)             (cons (assert-no-convert a)
                                          (assert-no-convert b))]
            [_                      x]))

        ;; Qualifies all names in thm with the theorem id (AKA the file name).
        ;; This way, the names will appear in all-replacements-closure.
        (define (qual-thm id thm)
           (replace-all (map (lambda (g)
                               (list g (prefix-name g id)))
                             (theorem-globals thm))
                        (assert-no-convert thm)))

        (write-to-out
          (format "~s"
            (make-immutable-hash
              (hash-map
                (benchmark-theorems)
                (lambda (id thm)
                  (cons id
                        (prefix-custom-destructors
                          (prefix-custom-constructors
                            (replace final-replacements
                                     (unqualify (qual-thm id thm)))))))))))
      '';

    BENCHMARKS_NORMALISED_DEFINITIONS = runRacket "normalised-definitions"
      env
      { inherit BENCHMARKS; }
      ''
        (require lib/impure)
        (require lib/normalise)
        (require lib/strip-native)

        (define (add-custom-defs x)
         (append x
                 (map (match-lambda
                        [(list def deps)
                         (prefix-locals def)])
                      (list custom-bool custom-ite custom-not custom-and
                            custom-or custom-=> custom-bool-converter custom-nat
                            custom-int custom-plus custom-inc custom-dec
                            custom-invert custom-abs custom-sign custom-+
                            custom-- custom-* custom-nat-> custom-> custom-div
                            custom-mod custom-< custom->= custom-<=))))

        (write-to-out
          (format "~s"
            (normed-and-replacements
              (add-custom-defs (first (qual-hashes-theorem-files))))))
      '';

    BENCHMARKS_FINAL_BENCHMARK_DEFS = runRacket "final-defs"
      env
      { inherit BENCHMARKS BENCHMARKS_NORMALISED_DEFINITIONS; }
      ''
        (require lib/impure)
        (require lib/normalise)
        (require lib/tip)
        (write-to-out
          (format "~s"
            (prepare (first (normed-and-replacements-cached)))))
      '';

    BENCHMARKS_ONLY_FUNCTION_NAMES = runRacket "function-names"
      env
      { inherit BENCHMARKS BENCHMARKS_FINAL_BENCHMARK_DEFS; }
      ''
        (require lib/impure)
        (require lib/conjectures)
        (require lib/lists)
        (require lib/normalise)
        (require lib/sampling)
        (require lib/theorems)

        (define lowercase-names
          (map decode-name (lowercase-benchmark-names)))

        (define all-constructors
          (strip-matching-prefix lowercase-names "constructor-"))

        (define all-destructors
          (strip-matching-prefix lowercase-names "destructor-"))

        (define artificial
          '(custom-bool-converter))

        (define unused
          '(grammars/packrat_unambigPackrat.smt2linA))

        (define only-function-names
          (remove* (append all-constructors all-destructors artificial)
                   lowercase-names))

        (write-to-out (format "~s" only-function-names))
      '';
  };
}
