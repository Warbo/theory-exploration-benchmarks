#!/usr/bin/env racket
#lang racket

(require (file "defs.rkt"))

(define (usage)
  (log "Usage: choose_sample <N> <M>\nWhere N and M are natural numbers.\nN \
is the size of the sample and M is the index of the sample. Samples are chosen \
deterministically (but unpredictably) using hashes, so M is required to get \
multiple samples of the same size.\n")
  (exit 1))

(unless (equal? 2 (vector-length (current-command-line-arguments)))
  (usage))

(define-values (size rep)
  (apply values (map string->number
                     (vector->list (current-command-line-arguments)))))

(when (or (equal? size #f) (equal? rep #f))
  (usage))

(define result
  (sample-from-benchmarks size rep))

(display (string-join (map symbol->string (set->list result))
                      "\n"))
