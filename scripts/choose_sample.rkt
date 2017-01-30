#!/usr/bin/env racket
#lang racket

(require (file "defs.rkt"))

(define (usage)
  (log "Usage: choose_sample <N> <M>\nWhere N and M are natural numbers.\nN \
is the size of the sample and M is the index of the sample. Samples are chosen \
deterministically (but unpredictably) using hashes, so M is required to get \
multiple samples of the same size.\n\nTo sample from equations only, set the \
environment variable 'EQUATIONS_ONLY' to '1'.")
  (exit 1))

(unless (equal? 2 (vector-length (current-command-line-arguments)))
  (log (format "Expected 2 arguments, given ~s"
               (current-command-line-arguments)))
  (usage))

(define-values (size rep)
  (apply values (map string->number
                     (vector->list (current-command-line-arguments)))))

(when (or (equal? size #f) (equal? rep #f))
  (log (format "Expected two integers, given ~s"
               `((size ,size) (rep ,rep))))
  (usage))

(define result
  (if (equal? (getenv "EQUATIONS_ONLY") "1")
      (sample-equational-from-benchmarks size rep)
      (sample-from-benchmarks size rep)))

(display (string-join (map symbol->string (set->list result))
                      "\n"))
