#lang racket

(provide memo0 memo1)

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
          (set! result (let () body ...))
          (set! called #t))
        result))))

;; Memoise a unary function
(define (memo1 init)
  (let ([results (make-hash)])
    (lambda (arg)
      (hash-ref! results
                 arg
                 (lambda () (init arg))))))
