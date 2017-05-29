#!/usr/bin/env racket
#lang racket
(require "lib/defs.rkt")

(display (decode-string (port->string (current-input-port))))
