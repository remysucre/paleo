#lang racket

(require "paleo.rkt" "conflict.rkt")

;; TO use: (synth R1 Psi <input list of ints> <output list of ints> *debug* *learn?*)
;; If *debug* flag is #t, intermediate information about the knowledge base and Partial program will be printed
;; If *learn?* flag is #t, the engine will try to learn extra incorrect constructs based on a MUC

;---------------------
; | Simple examples: |
;---------------------
; Test 1
(synth R1 Psi '(2) '(2) #f #f) ; No learning
(synth R1 Psi '(2) '(2) #f #t) ; Learning
; Test 2
(synth R1 Psi '(6 8) '(1) #f #f)
(synth R1 Psi '(6 8) '(1) #f #t)
; Test 3
(synth R1 Psi '(6 8) '(8) #f #f)
(synth R1 Psi '(6 8) '(8) #f #t)
; Test 4
(synth R1 Psi '(10 8 4) '(10 8 4) #f #f)
(synth R1 Psi '(10 8 4) '(10 8 4) #f #t)

;------------------------
; | Difficult examples: |
;------------------------
; Test 5
;(synth R1 Psi '(3 3) '(6) #f #f) ; Warning: Very long
(synth R1 Psi '(3 3) '(6) #f #t)
; Test 6
(synth R1 Psi '(5 6 7) '(6) #f #f)
(synth R1 Psi '(5 6 7) '(6) #f #t)