#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define given-symbols
  (port->lines (current-input-port)))

(define (theorem-files)
  (filter (lambda (x) (string-suffix? (path->string x) ".smt2"))
          (sequence->list (in-directory "modules/tip-benchmarks/benchmarks"))))

(define (symbols-of-theorem path)
  (benchmark-symbols (file->string path)))

(define (acceptable-theorem thm-path)
  (null? (remove* (cons "" given-symbols)
                  (map symbol->string (symbols-of-theorem thm-path)))))

(define (acceptable-theorems)
  (filter acceptable-theorem (theorem-files)))

(displayln (format-symbols (acceptable-theorems)))
