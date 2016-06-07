#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(require racket/trace)
(include "defs.rkt")

(define given-lines
  (filter (lambda (x) (not (equal? 0 (string-length x))))
          (port->lines (current-input-port))))

(define (mk-output line so-far name-replacements)
  (let* ([expr      (read-benchmark line)]
         [norm-line (norm     expr)]
         [names     (names-in expr)]
         [existing  (filter (lambda (x)
                              (equal? norm-line (second x)))
                            so-far)])
    (if (empty? existing)
        (list (cons (list names norm-line) so-far)
              name-replacements)
        (list so-far
              (append (zip names (first (car existing)))
                      name-replacements)))))

(define (zip xs ys)
  (if (empty? xs)
      null
      (if (empty? ys)
          null
          (cons (list (car xs) (car ys))
                (zip  (cdr xs) (cdr ys))))))

(define (remove-redundancies lines so-far name-replacements)
  (if (empty? lines)
      (list so-far name-replacements)
      (let* ([result (mk-output (car lines) so-far name-replacements)]
             [new-sf (first  result)]
             [new-nr (second result)])
        (remove-redundancies (cdr lines)
                             new-sf
                             new-nr))))

(define output
  (let* ([results           (remove-redundancies given-lines null null)]
         [so-far            (first results)]
         [name-replacements (second results)])
    (append (map (lambda (x)
                   (format "DEF\t~a\t~a"
                           (join-spaces (first x))
                           (second x)))
                 so-far)
            (map (lambda (x)
                   (format "NAME\t~a\t~a" (first x) (second x)))
                 name-replacements))))

(show output)
