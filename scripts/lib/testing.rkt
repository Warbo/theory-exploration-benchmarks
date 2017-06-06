#lang racket

;; Helper functions/macros for tests. Also re-exports things from RackUnit.

(require rackunit)
(require "impure.rkt")

(provide check-equal? check-exn check-false
         check-not-equal? check-pred check-true constructorS constructorZ
         custom-bool custom-int custom-ite custom-nat def-test-case form
         form-with-deps mut nat-def redundancies test-data test-files
         testing-file with-check-info)

;; Selects specific test-cases based on a regex from the environment
(define test-case-regex
  ;; Match everything if no regex given
  (let ([given (or (getenv "PLT_TEST_REGEX") "")])
    (match given
      ["" #rx".*"]
      [_  (regexp given)])))

;; This macro will define a test case if it matches a given regex, or skip it
;; otherwise. Use this in favour of raw rackunit, so tests are skippable.
(define-syntax-rule (def-test-case name body ...)
  (when (regexp-match? test-case-regex name)
    (eprintf (string-append name "\n"))
    (test-case name body ...)))

;; Loads test data from files
(define (test-data f)
  (when (member (getenv "TEST_DATA") '(#f ""))
    (error "No TEST_DATA env var given"))
  (string-append (getenv "TEST_DATA") "/" f))


;; Suppress progress info during tests, as it's verbose and confusing. This can
;; be bypassed by setting the DEBUG env var, but it's suggested to also set the
;; PLT_TEST_REGEX env var so you only get output from specific tests.
(quiet)

;; For testing, we default to only using a subset of the benchmarks, which we
;; accomplish by overriding theorem-files; this acts as a sanity check, and is
;; much faster than checking everything. For a thorough test of all benchmarks
;; there is a separate "tests" derivation in default.nix, suitable for use in
;; e.g. a continuous integration scenario.
;;
;; Please note the following:
;;  - If a particular set of benchmarks has specifically been requested, via
;;    the BENCHMARKS environment variable, we use that whole set, rather than
;;    selecting some subset.
;;  - If a test requires some particular file to be present in this list, it
;;    should use the testing-file function to check that it's present.
(define testing-file
  (let ()
    ;; We always include the following files, since they're either required by
    ;; one of our tests, or they're edge-cases/regressions which we want to
    ;; ensure are getting regularly tested.
    (define required-testing-files
      (benchmark-files '("grammars/packrat_unambigPackrat.smt2"
                         "isaplanner/prop_01.smt2"
                         "isaplanner/prop_15.smt2"
                         "isaplanner/prop_35.smt2"
                         "isaplanner/prop_43.smt2"
                         "isaplanner/prop_44.smt2"
                         "isaplanner/prop_84.smt2"
                         "prod/prop_35.smt2"
                         "tip2015/bin_plus_comm.smt2"
                         "tip2015/fermat_last.smt2"
                         "tip2015/heap_SortPermutes'.smt2"
                         "tip2015/list_SelectPermutations.smt2"
                         "tip2015/nat_pow_one.smt2"
                         "tip2015/propositional_AndCommutative.smt2"
                         "tip2015/propositional_AndIdempotent.smt2"
                         "tip2015/sort_NStoogeSort2Count.smt2"
                         "tip2015/sort_NStoogeSort2Permutes.smt2"
                         "tip2015/tree_sort_SortPermutes'.smt2")))

    ;; Override theorem-files to return these chosen files, if no BENCHMARKS
    ;; were given explicitly
    (when (member (getenv "BENCHMARKS") '(#f ""))
      (set-theorem-files! (lambda ()
                            required-testing-files)))

    ;; The definition of testing-file; checks if the given file is in our
    ;; selected list.
    (lambda (f)
      (unless (member (benchmark-file f) required-testing-files)
        (error "Testing file not in required list" f))
      f)))

;; Example data, useful across a variety of tests

(define nat-def
  '(declare-datatypes () ((Nat (Z) (S (p Nat))))))

(define constructorZ
  '(define-fun constructor-Z ()              Nat (as  Z          Nat)))

(define constructorS
  '(define-fun constructor-S ((local-p Nat)) Nat (as (S local-p) Nat)))

(define redundancies `(,constructorZ
                       ,constructorS
                       (define-fun redundantZ1 () Nat (as Z Nat))
                       (define-fun redundantZ2 () Nat (as Z Nat))
                       (define-fun redundantZ3 () Nat (as Z Nat))))

(define custom-nat
  '(declare-datatypes () ((CustomNat (CustomZ)
                                     (CustomS (custom-p CustomNat))))))

(define custom-int
  '(declare-datatypes () ((CustomInt (CustomNeg (custom-succ CustomNat))
                                     (CustomZero)
                                     (CustomPos (custom-pred CustomNat))))))

(define form-deps
  (list custom-nat custom-int))

(define form
  '(declare-datatypes ()
                      ((Form (& (&_0 Form) (&_1 Form))
                             (Not (Not_0 Form))
                             (Var (Var_0 CustomInt))))))

(define form-with-deps
  (append form-deps (list form)))

(define custom-bool
  '(declare-datatypes () ((CustomBool (CustomTrue) (CustomFalse)))))

(define custom-ite
  '(define-fun
     (par (a)
          (custom-ite
           ((c CustomBool) (x a) (y a)) a
           (match c
             (case CustomTrue  x)
             (case CustomFalse y))))))

(define mut (list custom-bool
                  custom-nat
                  custom-int
                  custom-ite
                  '(define-funs-rec
                     ((models  ((x CustomBool)
                                (y CustomInt))
                               CustomBool)
                      (models2 ((q CustomBool)
                                (x CustomInt))
                               CustomBool)
                      (models5 ((q CustomBool)
                                (x CustomInt)
                                (y CustomInt))
                               CustomBool))

                     ((custom-ite x
                                  (models2 (models x y) y)
                                  (models5 (models x y) y y))

                      (custom-ite q
                                  (models5 (models q x) x x)
                                  (models2 (models q x) x))

                      (custom-ite q
                                  (models2 q x)
                                  (models5 q x y))))))

(define test-files
  (benchmark-files '("grammars/simp_expr_unambig1.smt2"
                     "grammars/simp_expr_unambig4.smt2"
                     "tip2015/sort_StoogeSort2IsSort.smt2"
                     "tip2015/sort_BSortPermutes.smt2")))
