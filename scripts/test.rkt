#lang racket

;; Tests are defined in-line in the 'test' submodule of lib/*.rkt, which can be
;; run using 'raco test lib/*.rkt'
;;
;; We provide this module, which just executes those same tests, to make other
;; activities easier, such as profiling.
(require (submod "lib/defs.rkt" test))
