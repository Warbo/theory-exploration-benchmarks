#!/usr/bin/env racket
#lang racket

(require "lib/normalise.rkt")
(require "lib/tip.rkt")

(when (getenv "BENCHMARKS_FINAL_BENCHMARK_DEFS")
  (error "BENCHMARKS_FINAL_BENCHMARK_DEFS already set, aborting"))

(write (prepare (first (normed-and-replacements-cached))))
