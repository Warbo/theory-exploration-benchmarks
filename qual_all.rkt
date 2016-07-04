#!/usr/bin/env racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define given-files
  (port->lines (current-input-port)))

(define given-contents
  (map (lambda (pth)
         (list (string-replace
                (string-join
                 (reverse (take (reverse (string-split pth "/")) 2))
                 "/") "'" "_tick_")
               (read-benchmark (file->string pth))))
       given-files))

(define qualified-contents
  (map (lambda (name-content)
         (qualify (first name-content) (second name-content)))
       given-contents))

(show (apply append qualified-contents))
