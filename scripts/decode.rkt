#!/usr/bin/env racket
#lang racket
(require "lib/normalise.rkt")

(display (decode-string (port->string (current-input-port))))
