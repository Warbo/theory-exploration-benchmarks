#!/usr/bin/env racket
#lang racket

(require json)
(require lib/conjectures)

(write-json
 (precision-recall-eqs-wrapper (port->string)
                               (getenv "TRUTH_SOURCE")
                               (file->string (getenv "GROUND_TRUTH"))))
