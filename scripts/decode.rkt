#!/usr/bin/env racket
#lang racket
(require lib/normalise)

(display (decode-string (port->string (current-input-port))))
