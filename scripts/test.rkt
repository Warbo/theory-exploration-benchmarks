#lang racket

;; Tests are defined in-line in the 'test' submodule of defs.rkt, which can be
;; run using 'raco test defs.rkt'
;;
;; We provide this module, which just executes those same tests, to make other
;; activities easier, such as profiling.
(require (submod "defs.rkt" test))
