#!/usr/bin/env racket
#lang racket
(require racket/include)

(include "defs.rkt")

(define (format-benchmark-symbols)
  (format-symbols (benchmark-symbols (port->string (current-input-port)))))

(displayln (format-benchmark-symbols))
