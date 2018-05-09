#lang racket

(provide (all-defined-out))

; TODO long
(define (Decide P gamma phi Omega)
  '(1 2))

; TODO probably pretty short enumeration over the tree
(define (Holes P)
  '())

; TODO make sure this matches data structure
#;(define (Decide P gamma phi Omega)
    (define V (for/list ([H Holes(P)])
                (define p (get-production H))
                (cons H p)))
    (define V1 (filter V (lambda (x)
                           (Consistent? (Fill P (car x) (cdr x)) Omega))))
    (for/fold ([default #f]) ([x V1])
      (match-define (cons H p) x)
      (if (> (L x) (L default)) x default)))

; TODO long
(define (Propagate P gamma H p Omega)
  '())

; TODO long
(define (Unsat Omega)
  '())