#lang racket

(provide (all-defined-out))
(require "partial.rkt")

; TODO long
#;(define (Decide P gamma phi Omega)
  '(1 2))

; TODO probably pretty short enumeration over the tree
(define (Consistent? P Omega)
  #t)

(define (L x)
  (cond
    [(not x) 0]
    [(match (cdr x) [(list `(,n ,t ...)) #f] [else #t]) 2]
    [else 1]))

(define R (make-immutable-hash
           (list (cons 'N (append `,(range 0 11) '(x1 x2 x3 x4 x5) '((last L) (head L) (sum L) (maximum L) (minimum L))))
             (cons 'L (list '(take L N) '(filter L T) '(sort L) '(reverse L)))
             (cons 'T '(geqz leqz eqz)))))

; TODO make sure this matches data structure
(define (Decide P gamma phi Omega rules)
    (define V (for/list ([hole (Holes P)])
                (define hole-non (Partial-Non-Terminal (Lookup-By-ID P hole)))
                (for*/list ([(prod-non prod-terms) (in-dict rules)]
                           #:when (eq? hole-non prod-non)
                           [prod-term prod-terms])
                  (list hole prod-term))))
    (define V1 (for/list ([possibilities V])
                 (filter (lambda (x)
                           (Consistent? (Fill P (car x) (cdr x)) Omega))
                         possibilities)))
    (for*/fold ([default #f]) ([possibilities V1] [candidate possibilities])
      (match-define (list H p) candidate)
      (if (> (L candidate) (L default)) candidate default)))

; TODO long
#;(define (Propagate P gamma H p Omega)
  #f)

(define (Propagate P gamma H p Omega rules)
  (define P1 (Fill P H p))
  (define S (for*/list ([hole (Holes P)]
                        #:when (number? hole)
                        [(prod-non prod-terms) (in-dict rules)]
                        #:when (eq? (Partial-Non-Terminal (Lookup-By-ID P hole)) prod-non)
                        [prod-term prod-terms])
              (list hole prod-term)))
  (for/fold ([default P1]) ([x S])
    (match-define (list hole prod) x)
    (define hole-non (Partial-Non-Terminal (Lookup-By-ID P hole)))
    (define R (for*/list ([(prod-non prod-terms) (in-dict rules)]
                            #:when (eq? hole-non prod-non)
                            [prod-term prod-terms])
                (list hole prod-term)))
    (if (= H 5)
        (Propagate P1 gamma hole prod Omega rules)
        default)))

; TODO long
(define (Unsat Omega)
  #f)