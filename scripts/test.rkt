#lang racket

;; Tests are defined in-line in the 'test' submodule of lib/*.rkt, which can be
;; run using 'raco test lib/*.rkt'
;;
;; We provide this module, which just executes those same tests, to make other
;; activities easier, such as profiling.
(require (submod "lib/impure.rkt"       test))
(require (submod "lib/compare.rkt"      test))
(require (submod "lib/lists.rkt"        test))
(require (submod "lib/sets.rkt"         test))
(require (submod "lib/defs.rkt"         test))
(require (submod "lib/util.rkt"         test))
(require (submod "lib/replacements.rkt" test))
(require (submod "lib/tip.rkt"          test))
(require (submod "lib/normalise.rkt"    test))
(require (submod "lib/theorems.rkt"     test))
(require (submod "lib/sigs.rkt"         test))
(require (submod "lib/sampling.rkt"     test))
(require (submod "lib/conjectures.rkt"  test))
(module+ test)
