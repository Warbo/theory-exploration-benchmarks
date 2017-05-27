#lang racket

(provide memo0 memo1 memo2)

;; Macros for memoising functions. Memoised functions store their return values,
;; which can be returned immediately if called again with the same arguments.
;; Useful for long computations when RAM is cheap. NOTE: Recursive functions
;; will store all intermediate return values if memoised. To avoid this, you can
;; memoise a lambda which calls your recursive function, then only the top-level
;; argument/return value pairs will be stored.

;; Memoise a nullary function. Since there are no arguments, we write the body
;; inline, rather than requiring a lambda wrapper.
(define-syntax-rule (memo0 name body ...)
  (define name
    (let ([result #f]
          [called #f])
      (lambda ()
        (when (equal? #f called)
          (eprintf "Forced ~a\n" name)
          (set! result (let () body ...))
          (set! called #t)
          (eprintf "Finished ~a\n" name))
        result))))

;; Memoise a unary function
(define (memo1 init)
  (let ([results (make-hash)])
    (lambda (arg)
      (hash-ref! results
                 arg
                 (lambda () (init arg))))))

;; Memoise a binary function
(define (memo2 init)
  (let ([results (make-hash)])
    (lambda (arg1 arg2)
      (hash-ref! results
                 (list arg1 arg2)
                 (lambda () (init arg1 arg2))))))
