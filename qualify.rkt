#!/usr/bin/env racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define given
  (read-benchmark (port->string (current-input-port))))

(define name
  (string-replace (getenv "NAME") "'" "_tick_"))

(show (qualify name given))
