#lang racket

;; Lists of (old new) pairs, which represent replacing old with new. We model
;; these explicitly, since our normalisation algorithms require iteratively
;; extending these replacements. Having a nice little theory of replacements
;; makes this easy.

(require "lists.rkt")
(require "sets.rkt")
(require "util.rkt")

(provide extend-replacements)

(module+ test
  (require "testing.rkt"))

(define old-of (match-lambda [(list old new) old]))
(define new-of (match-lambda [(list old new) new]))

;; A replacement: a pair of symbols
(define (replacement? rep)
  (and (list?   rep)
       (equal?  (length rep) 2)
       (symbol? (old-of rep))
       (symbol? (new-of rep))))

(module+ test
  (define (compose-info f g)
    (lambda (x)
      (with-check-info
        (('x x))
        (f (g x)))))

  (def-test-case "Replacement"
    (for-each
     (compose-info check-true replacement?)
     '((A B) (foo bar) (some/path.smt2A-Name other/file.smt2Different)))
    (for-each
     (compose-info check-false replacement?)
     '(#t #f A () (A) ("A" B) (A "B") (A B C)))))

;; A list of replacements, satisfying a few consistency properties
(define (replacements? reps)
  (and (set? reps)
       (all-of replacement? (set->list reps))

       ;; No new value should also be an old value
       (set-empty? (set-filter (lambda (rep)
                                 (set-member? (set-map old-of reps)
                                              (new-of rep)))
                               reps))

       ;; All old values should have one new value
       (equal? (set-count (set-map old-of reps))
               (set-count reps))))

(module+ test
  (def-test-case "Replacements"
    (for-each
     (compose-info check-true replacements?)
     (list (set)                  ;; Empty is fine
           (set '(A B))           ;; Singleton is fine
           (set '(A B) '(C D))    ;; Disjoint replacements can't interfere
           (set '(A B) '(C B))))  ;; Reusing new values is fine

    (for-each
     (compose-info check-false replacements?)
     (list (set 'A 'B)              ;; Not a set of replacements
           (set '(A B) '(A C))      ;; Duplicate old values not allowed
           (set '(A B) '(B C))))))  ;; Multi-step replacement not allowed

(define/test-contract (insert-replacement reps rep)
  (-> replacements? replacement? replacements?)
  (match rep
    [(list old new) (set-add (set-map (match-lambda
                                        [(list t-old t-new)
                                         (list t-old (if (equal? old t-new)
                                                         new
                                                         t-new))])
                                      reps)
                             rep)]))

(module+ test
  (def-test-case "Insert replacement"
    ;; Nothing to check when empty, so inserted as-is
    (check-equal? (insert-replacement (set) '(A B))
                  (set '(A B)))

    ;; Disjoint replacements don't interfere
    (check-equal? (insert-replacement (set '(A B) '(C D)) '(E F))
                  (list->set '((E F) (A B) (C D))))))

(define/test-contract (extend-replacements xs ys)
  (->i ([xs replacements?]
        [ys replacements?])
       [result (xs ys)
               (let ()
                 (define old-xs  (set-map old-of xs))
                 (define new-xs  (set-map new-of xs))
                 (define old-ys  (set-map old-of ys))
                 (define new-ys  (set-map new-of ys))
                 (define old-out (set-map old-of result))
                 (define new-out (set-map new-of result))
                 (define old-in  (set-union old-xs old-ys))
                 (define new-in  (set-union new-xs new-ys))

               (and/c replacements?
                      ;; All old values should get replaced
                      (subset? old-in old-out)

                      ;; All of ys's replacements should be honoured
                      (subset? new-ys new-out)

                      ;; All old values should come from xs and ys
                      (subset? old-out old-in)

                      ;; All new values should come from xs and ys
                      (subset? new-out new-in)))])
  (set-foldl insert-replacement xs ys))
