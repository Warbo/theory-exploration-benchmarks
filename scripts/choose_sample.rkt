#!/usr/bin/env racket
#lang racket

(require lib/impure)
(require lib/sampling)

(match (map string->number
            (vector->list (current-command-line-arguments)))
  [(list #f   _)   (error  "size parameter should be a natural number")]
  [(list _    #f)  (error "index parameter should be a natural number")]
  [(list size rep) (display
                    (string-join (map symbol->string
                                      (set->list
                                       (sample-from-benchmarks size
                                                               rep)))
                                 "\n"))]
  [_               (error (string-join
                           '("Usage: choose_sample <size> <index>"
                             ""
                             "Where size and index are natural numbers."
                             ""
                             "size determines how many names will be included"
                             "in the sample, whilst different index values give"
                             "rise to different samples (using the same index"
                             "will give out the same sample).")
                           "\n"))])
