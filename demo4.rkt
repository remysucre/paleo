#lang racket

(require "paleo.rkt" "conflict.rkt")

(eprintf "In: ~s, out: ~s\n" '(3 3) '(6))(synth R1 Psi '(3 3) '(6) #t #t)