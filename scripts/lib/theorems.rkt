#lang racket

(require lib/impure)
(require lib/lists)
(require lib/memo)
(require lib/normalise)
(require lib/replacements)
(require lib/tip)
(require lib/util)

(provide all-theorem-deps benchmark-theorems normalised-theorems
         normed-theorem-of theorem-deps-of theorem-globals)

(module+ test
  (require lib/testing))

;; Extracts a list of theorem statements from a given benchmark file. For real
;; TIP benchmarks this should contain a single theorem, so we enforce this in
;; the contract. Many of our generated files (e.g. used for translating) do not
;; contain any theorems, and hence it makes no sense to pass them into this
;; function. There should never be multiple theorems.
;; NOTE: We also unwrap any calls to 'custom-bool-converter', which strip-native
;; adds in to work around TIP's special-casing of '='. Since no exploration
;; system will include such things in their output, we strip them here to ensure
;; comparisons will find correct matches.
(define/test-contract (theorem-from-file f)
  (-> any/c (lambda (result)
              (or (and (list? result)
                       (not (empty? result))
                       (equal? (first result) 'assert-not))
                  (raise-user-error
                   'result
                   "Expected a single (negated) theorem. Found ~a"
                   result))))
  (unwrap-custom-bool (second (hash-ref (theorem-hashes) f))))

(module+ test
  (def-test-case "Can extract theorems"
    (for-each (lambda (id)
                (define thm (theorem-from-file id))

                (define content (unwrap-custom-bool
                                 (file->list (benchmark-file id))))

                (with-check-info
                  (('benchmark-file id)
                   ('thm            thm)
                   ('content        content))
                  (check-not-equal? (member thm content) #f)))
              (theorem-ids))))

(memo0 benchmark-theorems
       (make-immutable-hash
        (foldl (lambda (f rest)
                 (cons (cons f (theorem-from-file f))
                       rest))
               '()
               (theorem-ids))))

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
              (theorem-ids))))

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
      [(list 'forall vars body) (append (append-map (lambda (var)
                                                      (symbols-in (second var)))
                                                    vars)
                                        (symbols-in body))]
      [(list 'par _ body)       (thm-names body)]
      [(cons x y)               (append (thm-names x) (thm-names y))]
      [_                        (symbols-in expr)]))

    (remove* (thm-locals thm) (thm-names thm)))

(define (unwrap-custom-bool thm)
  (match thm
    [(list 'custom-bool-converter x) (unwrap-custom-bool x)]
    [(cons x y)                      (cons (unwrap-custom-bool x)
                                           (unwrap-custom-bool y))]
    [x                               x]))

(memo0 normalised-theorems
  (read-from-cache! "BENCHMARKS_NORMALISED_THEOREMS"))

(module+ test
  (def-test-case "Theorem names get normalised"
    (hash-for-each (normalised-theorems)
                   (lambda (f thm)
                     (for-each (lambda (sym)
                                 (define norm-sym (nn sym))

                                 (with-check-info
                                   (('thm      thm)
                                    ('f        f)
                                    ('sym      sym)
                                    ('norm-sym norm-sym))
                                   (check-equal? norm-sym sym
                                                 "Theorem global is normal")))
                               (symbols-in thm)))))

  (def-test-case "No custom-bool-converter in theorems"
    (hash-for-each (normalised-theorems)
                   (lambda (_ thm)
                     (define syms (symbols-in thm))

                     (with-check-info
                       (('theorem thm)
                        ('symbols syms))
                       (check-false (member 'custom-bool-converter syms)))))))

(define/test-contract (normed-theorem-of f)
  (-> tip-path? theorem?)
  (hash-ref (normalised-theorems) f
            (lambda ()
              (raise-arguments-error
               'normed-theorem-of
               "No theorem found"
               "id" f))))

(module+ test
  (def-test-case "Normalise theorems"
    (define (structure-of expr)
      (match expr
        [(cons x y) (cons (structure-of x) (structure-of y))]
        [_          #f]))

    (for-each (lambda (theorem-id)
                (define unnormed
                  (theorem-of theorem-id))
                (define normed
                  (normed-theorem-of theorem-id))

                (check-equal? (structure-of unnormed) (structure-of normed)))
              (theorem-ids))))

(define (theorem-types expr)
  (match expr
    [(list 'forall vars body) (append (theorem-types body)
                                      (append-map (lambda (var)
                                                    (symbols-in (second var)))
                                                  vars))]
    [(list 'as x t)           (append (theorem-types x)
                                      (symbols-in t))]
    [(list 'lambda vars body) (append (theorem-types body)
                                      (append-map (lambda (var)
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

           (remove-duplicates
            (foldl (lambda (name existing)
                     ;; Prefix constructors, so we use the function instead
                     (cons (if (member name constructors)
                               (prefix-name name "constructor-")
                               name)
                           existing))
                   '()
                   raw-names)))))

(module+ test
  (def-test-case "Expected dependencies"
    (define deps
      (theorem-deps-of "isaplanner/prop_84.smt2"))

    (with-check-info
      (('deps deps)
       ('should-have '(zip append take len drop)))
      (check-equal? (length deps) 5)))

  (def-test-case "Theorem deps"
    (for-each (lambda (name-cases)
                (for-each (match-lambda
                            [(list f raw-deps)
                             (define deps
                               (replace-names raw-deps))

                             (define calc-deps
                               (replace-names
                                (theorem-deps-of f)))

                             (with-check-info
                               (('sort      (first name-cases))
                                ('f         f)
                                ('raw-deps  raw-deps)
                                ('deps      deps)
                                ('calc-deps calc-deps))
                               (check-equal? (list->set calc-deps)
                                             (list->set deps)))])
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
                      (testing-file "tip2015/propositional_AndCommutative.smt2"))
                   (tip2015/propositional_AndCommutative.smt2valid
                    constructor-tip2015/propositional_AndCommutative.smt2&))

                  (,(begin
                      (testing-file "prod/prop_35.smt2")
                      (testing-file "isaplanner/prop_01.smt2")
                      (testing-file "isaplanner/prop_15.smt2"))
                   (isaplanner/prop_15.smt2len isaplanner/prop_15.smt2ins
                    constructor-CustomS))
                  (,(begin
                      (testing-file "isaplanner/prop_35.smt2"))
                   (constructor-CustomFalse
                    isaplanner/prop_35.smt2dropWhile))))))))

(memo0 all-theorem-deps
       (map (lambda (f) (list f (list->set (theorem-deps-of f))))
            (theorem-ids)))
