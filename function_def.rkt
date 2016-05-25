#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define (defs-from exp)
  (match exp
         [(list 'define-fun     name args typ body) (if (equal? (symbol->string name)
                                                                given-symbol)
                                                        (list (list name
                                                                    args
                                                                    typ
                                                                    body))
                                                        null)]
         [(list 'define-fun-rec name args typ body) (if (equal? (symbol->string name)
                                                                given-symbol)
                                                        (list (list name
                                                                    args
                                                                    typ
                                                                    body))
                                                        null)]
         [(cons a b)                  (append (defs-from a)
                                              (defs-from b))]
         [_                           null]))

(define given-symbol
  (car (port->lines (current-input-port))))

(define files-with-given
  (filter (lambda (path)
            (member given-symbol (map symbol->string (symbols-of-theorem path))))
          (theorem-files)))

(define defs-of-given
  (foldl (lambda (path rest)
           (append (defs-from (read-benchmark (file->string path)))
                   rest))
         '()
         files-with-given))

(define unique-defs
  (remove-duplicates defs-of-given))

(define (show x)
  (displayln (format-symbols x)))

(show unique-defs)
