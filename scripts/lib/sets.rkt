#lang racket

(require "util.rkt")

;; Functions for operating on sets

(module+ test
  (require rackunit))

;; Map a function F over the elements of a set S
(define (map-set f s)
  (list->set (set-map s f)))

(define/test-contract (precision found wanted)
  (-> set? set? rational?)
  (/ (set-count (set-intersect found wanted))
     (set-count found)))

(module+ test
  (test-case "Precision"
             (check-equal?
              (/ 1 10)
              (precision (list->set '(a b c d e f g h i j))
                         (list->set '(j k l m n o p q r s t u v w x y z))))))

(define/test-contract (recall found wanted)
  (-> set? set? rational?)
  (/ (set-count (set-intersect wanted found))
     (set-count wanted)))

(module+ test
  (test-case "Recall"
             (check-equal? (/ 1 2)
                           (recall (list->set '(a b c d e f g h i j k l m))
                                   (list->set '(a b c d e f g h i j k l m
                                                  n o p q r s t u v w x y z))))))
