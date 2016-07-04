#!/usr/bin/env racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define given-defs
  (map read-benchmark (port->lines (current-input-port))))

(show (apply append (map names-in given-defs)))
