#lang racket

;; Functions for operating on sets (unordered lists with no duplicates)

(require lib/util)
(provide precision recall set-filter set-foldl set-map)

(module+ test
  (require lib/testing))

;; Map a function F over the elements of a set S. This shadows Racket's built-in
;; set-map, which unhelpfully returns a list (and has dodgy argument order)
(define (set-map f s)
  (list->set (map f (set->list s))))



(define/test-contract (precision found wanted)
  (-> set? set? rational?)
  (/ (set-count (set-intersect found wanted))
     (set-count found)))

(module+ test
  (def-test-case "Precision"
    (check-equal?
     (/ 1 10)
     (precision (list->set '(a b c d e f g h i j))
                (list->set '(j k l m n o p q r s t u v w x y z))))))

(define/test-contract (recall found wanted)
  (-> set? set? rational?)
  (/ (set-count (set-intersect wanted found))
     (set-count wanted)))

(module+ test
  (def-test-case "Recall"
    (check-equal? (/ 1 2)
                  (recall (list->set '(a b c d e f g h i j k l m))
                          (list->set '(a b c d e f g h i j k l m
                                         n o p q r s t u v w x y z))))))

(define (set-filter f s)
  (list->set (filter f (set->list s))))

(define (set-foldl f init s)
  (foldl f init (set->list s)))
