#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define (given-symbols)
    (port->lines (current-input-port)))

(define (theorem-files)
  (filter (lambda (x) (string-suffix? (path->string x) ".smt2"))
          (sequence->list (in-directory "modules/tip-benchmarks/benchmarks"))))

(theorem-files)

;;(define (acceptable-theorem t)
;;     ./symbols_of_theorems.scm < "$1" | while read -r REQUIRED
;;     do
;;         [[ -n "$REQUIRED" ]] || continue # Skip blank lines
;;         if given_symbols | grep -Fx "$REQUIRED" > /dev/null
;;         then
;;             # We're given the required symbol
;;             continue
;;         fi
;;         echo "Skipping '$1' as we don't have '$REQUIRED'" 1>&2
;;         return 1
;;     done
;;     return 0
;; }

;; echo -e "Given symbols:\n$SYMS" 1>&2

;; theorem_files | while read -r THEOREM
;; do
;;     echo "Checking '$THEOREM'" 1>&2
;;     if acceptable_theorem "$THEOREM"
;;     then
;;         echo "$THEOREM"
;;     fi
;; done

;; echo "Done" 1>&2
