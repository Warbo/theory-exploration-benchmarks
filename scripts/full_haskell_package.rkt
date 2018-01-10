#!/usr/bin/env racket
#lang racket

(require lib/sigs)
(full-haskell-package-s (port->string (current-input-port))
                        (getenv "OUT_DIR"))
