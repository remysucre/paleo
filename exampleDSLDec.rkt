#lang racket

(define-production <non-terminal>
  [<terminal> <spec>] ...)

(define-program p
  [terminal or hole])

(run-synthesizer p)