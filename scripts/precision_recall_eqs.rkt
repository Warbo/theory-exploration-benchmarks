#!/usr/bin/env racket
#lang racket

(require json)
(require "lib/conjectures.rkt")

(write-json
 (precision-recall-eqs-wrapper (port->string)
                               (getenv "TRUTH_SOURCE")
                               (file->string (getenv "GROUND_TRUTH"))))
