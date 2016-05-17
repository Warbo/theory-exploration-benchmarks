#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket
(require racket/include)

(include "defs.rkt")

(define (format-symbols syms)
  (if (null? syms)
      ""
      (format "~a\n~a" (car syms) (format-symbols (cdr syms)))))

(define (format-benchmark-symbols)
  (format-symbols (benchmark-symbols (port->string (current-input-port)))))

(displayln (format-benchmark-symbols))
