#lang racket

(require "impure.rkt")
(require "lists.rkt")
(require "memo.rkt")
(require "normalise.rkt")
(require "tip.rkt")
(require "util.rkt")

(provide all-theorem-deps normalised-theorems normed-theorem-of theorem-deps-of)

(module+ test
  (require "testing.rkt"))

;; Extracts a list of theorem statements from a given benchmark file. For real
;; TIP benchmarks this should contain a single theorem, so we enforce this in
;; the contract. Many of our generated files (e.g. used for translating) do not
;; contain any theorems, and hence it makes no sense to pass them into this
;; function. There should never be multiple theorems.
;; NOTE: We also unwrap any calls to 'custom-bool-converter', which strip-native
;; adds in to work around TIP's special-casing of '='. Since no exploration
;; system will include such things in their output, we strip them here to ensure
;; comparisons will find correct matches.
(define/test-contract (theorems-from-file f)
  (-> any/c (lambda (result)
              (or (and (list? result)
                       (equal? (length result) 1))
                  (raise-user-error
                   'result
                   "Expected a single (negated) theorem. Found ~a"
                   result))))

  (define (get-theorems x)
    (match x
      [(list 'assert-not _) (list (unwrap-custom-bool x))]
      [(cons h t)           (append (get-theorems h) (get-theorems t))]
      [_                    '()]))

  (get-theorems (file->list f)))

(module+ test
  (def-test-case "Can extract theorems"
    (for-each (lambda (benchmark-file)
                (define thms (theorems-from-file benchmark-file))

                (define content (unwrap-custom-bool
                                 (file->list benchmark-file)))

                (with-check-info
                  (('benchmark-file benchmark-file)
                   ('thms           thms))
                  (check-equal? (length thms) 1))

                (with-check-info
                  (('benchmark-file benchmark-file)
                   ('thms           thms)
                   ('content        content))
                  (check-not-equal? (member (car thms) content) #f)))
              (append (theorem-files) (theorem-files)))))

(memo0 benchmark-theorems
       (make-immutable-hash
        (foldl (lambda (f rest)
                 (cons (cons (path-end f) (first (theorems-from-file f)))
                       rest))
               '()
               (theorem-files))))

(define (theorem-of f)
  (hash-ref (benchmark-theorems) (path-end f)
            (lambda ()
              (raise-arguments-error
               'theorem-of
               "No theorem found"
               "given-file" f
               "benchmark-dir (from BENCHMARKS)" benchmark-dir
               "benchmark-theorems" (benchmark-theorems)))))

(module+ test
  (def-test-case "Have benchmark theorems"
    (for-each (lambda (benchmark-file)
                (check-not-equal? (theorem-of benchmark-file)
                                  #f))
              (theorem-files))))

(define (theorem-globals thm)
  (define (thm-locals expr)
    (match expr
      [(list 'assert-not x)     (thm-locals x)]
      [(list 'par params body)  (append params (thm-locals body))]
      [(list 'forall vars body) (append (map first vars)
                                        (thm-locals body))]
      [(cons x y)               (append (thm-locals x) (thm-locals y))]
      [_                        '()]))

  (define (thm-names expr)
    (match expr
      [(list 'assert-not x)     (thm-names x)]
      [(list 'forall vars body) (append (concat-map (lambda (var)
                                                      (symbols-in (second var)))
                                                    vars)
                                        (symbols-in body))]
      [(list 'par _ body)       (thm-names body)]
      [(cons x y)               (append (thm-names x) (thm-names y))]
      [_                        (symbols-in expr)]))

    (remove* (thm-locals thm) (thm-names thm)))

(define (qual-thm filename thm)
  (replace-all (map (lambda (g)
                      (list g
                            (string->symbol (string-append (path-end filename)
                                                           (symbol->string g)
                                                           "-sentinel"))))
                    (theorem-globals thm))
               thm))

(define (unwrap-custom-bool thm)
  (match thm
    [(list 'custom-bool-converter x) (unwrap-custom-bool x)]
    [(cons x y)                      (cons (unwrap-custom-bool x)
                                           (unwrap-custom-bool y))]
    [x                               x]))

(memo0 normalised-theorems
  (if (member (getenv "BENCHMARKS_NORMALISED_THEOREMS") '(#f ""))
      ;; No cache given, generate
      (let ()
        (define qualified (qual-hashes-theorem-files))

        ;; First get replacements used in definitions
        (define replacements
          (replacements-closure qualified))

        ;; Also replace constructors with constructor functions, skipping
        ;; constructors which are redundant

        (define all-constructors
          (expression-constructors qualified))

        (define constructor-replacements
          (map (lambda (c)
                 (list c (prefix-name c "constructor-")))
               (remove* (map first replacements)
                        all-constructors)))

        ;; Update replacements to use constructor functions rather than
        ;; constructors
        (define final-replacements
          (append constructor-replacements
                  (map (lambda (rep)
                         (if (member (second rep)
                                     (map first constructor-replacements))
                             (list (first rep) (prefix-name (second rep)
                                                            "constructor-"))
                             rep))
                       replacements)))

        (make-immutable-hash
         (hash-map (benchmark-theorems)
                   (lambda (f thm)
                     (cons (path-end f)
                           (unqualify
                            (replace-all final-replacements
                                         (qual-thm (string-append
                                                    benchmark-dir "/"
                                                    (path-end f))
                                                   thm))))))))
      ;; Otherwise return cached version
      (read (file->string (getenv "BENCHMARKS_NORMALISED_THEOREMS")))))

(module+ test
  (def-test-case "No custom-bool-converter in theorems"
    (hash-for-each (normalised-theorems)
                   (lambda (_ thm)
                     (define syms (symbols-in thm))

                     (with-check-info
                       (('theorem thm)
                        ('symbols syms))
                       (check-false (member 'custom-bool-converter syms)))))))

(define (normed-theorem-of f)
  (hash-ref (normalised-theorems) (path-end f)
            (lambda ()
              (raise-arguments-error
               'normed-theorem-of
               "No theorem found"
               "given-file" f))))

(module+ test
  (def-test-case "Normalise theorems"
    (define (structure-of expr)
      (match expr
        [(cons x y) (cons (structure-of x) (structure-of y))]
        [_          #f]))

    (for-each (lambda (benchmark-file)
                (define unnormed
                  (theorem-of benchmark-file))
                (define normed
                  (normed-theorem-of benchmark-file))

                (check-equal? (structure-of unnormed) (structure-of normed))

                (check-false (string-contains? (format-symbols normed)
                                               "-sentinel")))
              (theorem-files))))

(define (theorem-types expr)
  (match expr
    [(list 'forall vars body) (append (theorem-types body)
                                      (concat-map (lambda (var)
                                                    (symbols-in (second var)))
                                                  vars))]
    [(list 'as x t)           (append (theorem-types x)
                                      (symbols-in t))]
    [(list 'lambda vars body) (append (theorem-types body)
                                      (concat-map (lambda (var)
                                                    (symbols-in (second var)))
                                                  vars))]
    [(cons x y)               (append (theorem-types x) (theorem-types y))]
    [_                        '()]))

(define theorem-deps-of
  (memo1 (lambda (f)
           (define normed (normed-theorem-of f))

           (define constructors
             (expression-constructors (normed-qualified-theorem-files)))

           ;; Remove types
           (define raw-names
             (remove* (theorem-types normed) (theorem-globals normed)))

           (define result
             (remove-duplicates
              (foldl (lambda (name existing)
                       ;; Prefix constructors, so we use the function instead
                       (cons (if (member name constructors)
                                 (prefix-name name "constructor-")
                                 name)
                             existing))
                     '()
                     raw-names)))
           result)))

(module+ test
  (def-test-case "Expected dependencies"
    (let ([deps (theorem-deps-of
                 (benchmark-file "tip2015/bin_plus_comm.smt2"))])
      (with-check-info
        (('deps deps))
        (check-equal? (length deps) 1))))

  (def-test-case "Theorem deps"
    (for-each (lambda (name-cases)
                (for-each (lambda (f-deps)
                            (define absolute-path
                              (benchmark-file (first f-deps)))

                            (define (prefix s)
                              (string->symbol
                               (string-append (first f-deps)
                                              (symbol->string s))))

                            (define calc-deps
                              (theorem-deps-of absolute-path))

                            (with-check-info
                              (('sort          (first name-cases))
                               ('absolute-path absolute-path)
                               ('deps          (second f-deps))
                               ('calc-deps     calc-deps))
                              (check-equal? (list->set calc-deps)
                                            (list->set (second f-deps)))))
                          (second name-cases)))

              ;; Cases are grouped by type, e.g. whether they require
              ;; constructor function replacement.
              ;; Each case has a filename and a list of expected dependencies;
              ;; we use testing-file to ensure these files are included in the
              ;; subset of files we're testing with. Since some names might
              ;; normalise differently in the presence of files with
              ;; lexicographically-smaller paths, we also ensure the canonical
              ;; definitions are included (inside the begin blocks).
              `(("Simple"
                 ((,(begin
                      (testing-file "tip2015/list_SelectPermutations.smt2")
                      (testing-file "tip2015/sort_NStoogeSort2Count.smt2")
                      (testing-file "tip2015/sort_NStoogeSort2Permutes.smt2"))
                   (tip2015/list_SelectPermutations.smt2isPermutation
                    tip2015/sort_NStoogeSort2Count.smt2nstoogesort2))

                  (,(begin
                      (testing-file "tip2015/heap_SortPermutes'.smt2")
                      (testing-file "tip2015/tree_sort_SortPermutes'.smt2"))
                   ,(list (quote |tip2015/heap_SortPermutes'.smt2isPermutation|)
                          (quote |tip2015/tree_sort_SortPermutes'.smt2tsort|)))))

                ("With constructors"
                 ((,(testing-file "tip2015/propositional_AndCommutative.smt2")
                   (tip2015/propositional_AndCommutative.smt2valid
                    constructor-tip2015/propositional_AndCommutative.smt2&))

                  (,(begin
                      (testing-file "tip2015/propositional_AndCommutative.smt2")
                      (testing-file "tip2015/propositional_AndIdempotent.smt2"))
                   (tip2015/propositional_AndCommutative.smt2valid
                    constructor-tip2015/propositional_AndCommutative.smt2&))

                  (,(begin
                      (testing-file "prod/prop_35.smt2")
                      (testing-file "isaplanner/prop_01.smt2")
                      (testing-file "tip2015/nat_pow_one.smt2"))
                   (prod/prop_35.smt2exp
                    constructor-isaplanner/prop_01.smt2S
                    constructor-isaplanner/prop_01.smt2Z))
                  (,(begin
                      (testing-file "isaplanner/prop_35.smt2"))
                   (constructor-CustomFalse
                    isaplanner/prop_35.smt2dropWhile))))))))

(memo0 all-theorem-deps
       (map (lambda (f) (list (path-end f) (list->set (theorem-deps-of f))))
            (theorem-files)))