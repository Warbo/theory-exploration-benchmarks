#lang racket
(require grommet/crypto/hash/sha256)
(require json)
(require racket/contract)
(require racket/contract/combinator)
(require racket/function)
(require racket/match)
(require racket/trace)
(require shell/pipeline)
(require lib/compare)
(require lib/impure)
(require lib/memo)
(require lib/util)

;; General infrastructure setup, not problem-specific

;; Set up our testing framework. We declare a bunch of integration tests at the
;; end of this file, but we also try to write unit tests next to each function's
;; definition and documentation. All testing goes in the 'test' submodule.
(module+ test
  (require rackunit)
  (require lib/testing))

;; Globals

;; Remove TIP boilerplate
(define (trim lst)
  (filter (lambda (x)
            (and (not (equal? (first x) 'assert-not))
                 (not (equal? x '(check-sat)))))
          lst))

(module+ test
  (def-test-case "Can trim"
    (check-equal? (trim '((hello)))                      '((hello)))
    (check-equal? (trim '((foo) (assert-not bar) (baz))) '((foo) (baz)))
    (check-equal? (trim '((foo) (check-sat) (bar)))      '((foo) (bar)))))
