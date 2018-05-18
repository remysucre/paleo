#lang racket

(provide (all-defined-out))
(require "partial.rkt")

; TODO long
(define (Decide P gamma phi Omega)
  '(1 2))

; TODO probably pretty short enumeration over the tree
(define (Consistent? P Omega)
  #f)

(define (Fill P H p)
  #f)

; TODO make sure this matches data structure
#;(define (Decide P gamma phi Omega)
    (define V (for/list ([H (Holes P)])
                (define p (get-production H))
                (cons H p)))
    (define V1 (filter V (lambda (x)
                           (Consistent? (Fill P (car x) (cdr x)) Omega))))
    (for/fold ([default #f]) ([x V1])
      (match-define (cons H p) x)
      (if (> (L x) (L default)) x default)))

; TODO long
(define (Propagate P gamma H p Omega)
  #f)

(define (Valid? p N)
  #f)

#;(define (Propagate P gamma H p Omega)
    (define P1 (Fill P H p))
    (define S (for/list ([N (Holes P)] [p gamma]) #:when (Valid? p N)
                (cons N p)))
    (for/fold ([default P1]) ([x S])
      (define R (for/list ([p (Productions gamma)])
                  #:when (Valid? p H) p))
      (if (...) (Propagate default gamma H p Omega) default)))

; TODO long
(define (Unsat Omega)
  #f)