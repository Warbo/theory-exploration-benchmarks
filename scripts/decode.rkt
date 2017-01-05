#!/usr/bin/env racket
#lang racket
(require "defs.rkt")

(display (decode-string (port->string (current-input-port))))
