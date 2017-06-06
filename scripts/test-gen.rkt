#lang racket

;; Test generating a smaller version of the benchmark. Useful for profiling.
(require "lib/impure.rkt")
(require "lib/sampling.rkt")

(set-theorem-files! (lambda ()
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
                                         "tip2015/tree_sort_SortPermutes'.smt2"))))

(write (make-sampling-data))
