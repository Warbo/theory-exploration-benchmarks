#!/usr/bin/env racket
#lang racket
;; Replaced (assoc-get 'normalised-theorems (get-sampling-data))
;; Write normalised-theorems return value to BENCHMARKS_NORMALISED_THEOREMS
(require "lib/theorems.rkt")
(write (normalised-theorems))
