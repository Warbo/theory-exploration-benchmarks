#!/usr/bin/env racket

(require "defs.rkt")

(display (decode-string (port->string (current-input-port))))
