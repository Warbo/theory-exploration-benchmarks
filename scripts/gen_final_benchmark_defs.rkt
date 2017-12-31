#!/usr/bin/env racket
#lang racket

(require lib/normalise)
(require lib/tip)

(module+ main
  (when (getenv "BENCHMARKS_FINAL_BENCHMARK_DEFS")
    (error "BENCHMARKS_FINAL_BENCHMARK_DEFS already set, aborting"))

  (write (prepare (first (normed-and-replacements-cached)))))
