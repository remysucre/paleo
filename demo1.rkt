#lang racket

(require "paleo.rkt" "conflict.rkt")

(eprintf "In: ~s, out: ~s\n" '(2) '(2))(synth R1 Psi '(2) '(2) #f #t)