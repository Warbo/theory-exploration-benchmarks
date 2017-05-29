#!/usr/bin/env racket
#lang racket

(require "lib/defs.rkt")

;; Generates data about renaming, etc. which can be cached and re-used to make
;; sampling and querying quicker.
(write (make-sampling-data))
