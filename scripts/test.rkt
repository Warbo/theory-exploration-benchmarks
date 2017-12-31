#lang racket

;; Tests are defined in-line in the 'test' submodule of lib/*.rkt, which can be
;; run using 'raco test lib/*.rkt'
;;
;; We provide this module, which just executes those same tests, to make other
;; activities easier, such as profiling.
(require (submod lib/impure       test))
(require (submod lib/compare      test))
(require (submod lib/lists        test))
(require (submod lib/sets         test))
(require (submod lib/defs         test))
(require (submod lib/util         test))
(require (submod lib/replacements test))
(require (submod lib/tip          test))
(require (submod lib/normalise    test))
(require (submod lib/theorems     test))
(require (submod lib/sigs         test))
(require (submod lib/sampling     test))
(require (submod lib/conjectures  test))
(module+ test)
