#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket
(require racket/include)

(include "defs.rkt")

(define (format-benchmark-symbols)
  (format-symbols (symbols-in (benchmark-types (port->string (current-input-port))))))

(displayln (format-benchmark-symbols))
