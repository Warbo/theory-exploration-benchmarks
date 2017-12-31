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

;; We cache an awful lot of stuff, so it's easy to cause a loop in our data flow
;; where generating values to be cached ends up indirectly looking for values in
;; the cache! To mitigate this, we provide the following functions:
;;
;;  generating? tells us if we're currently generating data for the cache
;;  start-generating! tells the program that it's now generating data
;;  stop-generating!  tells the program that it's no longer generating data
;;
;; When we want a function's results to be cached, we can give it a simple
;; if/then/else check to see if we're generating: if so, it should do the hard
;; work and generate the actual data; if not, it can pull it from the cache.
;;
;; Callers of our function then don't need to care if the data will generated or
;; taken from the cache; hence there's no need to update call sites when adding
;; caching to an existing function.
(define-values (generating? start-generating! stop-generating!)
  (let ([are-generating #f])
    (values
     (lambda () are-generating)

     (lambda ()
       (set! are-generating #t))

     (lambda ()
       (set! are-generating #f)))))
