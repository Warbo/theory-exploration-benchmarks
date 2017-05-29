#!/usr/bin/env racket
#lang racket

(require "lib/defs.rkt")

(write-json
 (precision-recall-eqs-wrapper (port->string)
                               (getenv "TRUTH_SOURCE")
                               (getenv "GROUND_TRUTH")))
