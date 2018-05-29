#lang racket

(provide (all-defined-out))
(require "partial.rkt" "SATsolver.rkt")

; TODO long
#;(define (Decide P gamma phi Omega)
  '(1 2))

; TODO probably pretty short enumeration over the tree
(define (Consistent? P omega rules)
  (define prod->unique (Make-prod->unique rules))
  (define num-prods (hash-count prod->unique))

  (define pi
    (for/list ([node P] #:when (Partial-Filled? node))
      `(,(+ (Node->Unique num-prods node)
           (Prod->Unique prod->unique (Partial-Terminal node))))))

  (define conflicts
    (for/list ([clause omega])
      (for/list ([literal clause])
        `,(-
           (+ (Node->Unique num-prods (Lookup-By-ID P (car literal)))
              (Prod->Unique prod->unique (Production-Terminal (cdr literal))))))))
  (define solution (solve (append pi conflicts)))
  solution)

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
                  (cons hole prod-term))))
    (define V1 (for/list ([possibilities V])
                 (filter (lambda (x)
                           (Consistent? (Fill P (car x) (cdr x)) Omega rules))
                         possibilities)))
    (for*/fold ([default #f]) ([possibilities V1] [candidate possibilities])
      (match-define (cons H p) candidate)
      (if (> (L candidate) (L default)) candidate default)))

;Test
; (eprintf "~s\n" (Decide P1 #f #f (list (list (cons 4 'geqz) (cons 2 'filter)) (list (cons 4 'leqz) (cons 2 'filter))) R))

; (Decide P1 #f #f (list (list (cons 4 'geqz) (cons 2 'filter)) (list (cons 4 'leqz) (cons 2 'filter))) R)

; TODO long
(define (Implied P H p omega rules)
  (define prod->unique (Make-prod->unique rules))
  (define num-prods (hash-count prod->unique))

  (define pi
    (for/list ([node P] #:when (Partial-Filled? node))
      `(,(+ (Node->Unique num-prods node)
           (Prod->Unique prod->unique (Partial-Terminal node))))))

  (define conflicts
    (for/list ([clause omega])
      (for/list ([literal clause])
        `,(-
           (+ (Node->Unique num-prods (Lookup-By-ID P (car literal)))
              (Prod->Unique prod->unique (Production-Terminal (cdr literal))))))))

  (define h-node (Lookup-By-ID P H))
  
  (define possible
    (for/list ([(prod-non prod-terms) (in-dict rules)]
                #:when (eq? (Partial-Non-Terminal h-node) prod-non))
      (for/list ([prod-term prod-terms])
        `,(+ (Node->Unique num-prods h-node)
             (Prod->Unique prod->unique (Production-Terminal prod-term))))))

  (define invert (list (- (+ (Node->Unique num-prods h-node)
                             (Prod->Unique prod->unique (Production-Terminal p))))))

  (define solution (solve (cons invert (append pi conflicts possible))))
  (not solution))

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
    (if (Implied P hole prod Omega rules)
        (Propagate P1 gamma hole prod Omega rules)
        default)))

; Test
;(print-partial (Propagate P1 #f 5 0 (list (list (cons 4 'geqz) (cons 2 'filter)) (list (cons 4 'leqz) (cons 2 'filter))) R))

; TODO long
(define (Unsat P omega rules)
  (define prod->unique (Make-prod->unique rules))
  (define num-prods (hash-count prod->unique))

  (define pi
    (for/list ([node P] #:when (Partial-Filled? node))
      `(,(+ (Node->Unique num-prods node)
           (Prod->Unique prod->unique (Partial-Terminal node))))))

  (define conflicts
    (for/list ([clause omega])
      (for/list ([literal clause])
        `,(-
           (+ (Node->Unique num-prods (Lookup-By-ID P (car literal)))
              (Prod->Unique prod->unique (Production-Terminal (cdr literal))))))))
  
  (define possible
    (for*/list ([node P] #:when (Hole? node) [(prod-non prod-terms) (in-dict rules)]
                #:when (eq? (Partial-Non-Terminal node) prod-non))
      (for/list ([prod-term prod-terms])
        `,(+ (Node->Unique num-prods node)
             (Prod->Unique prod->unique (Production-Terminal prod-term))))))

  (define solution (solve (append pi conflicts possible)))
  (not solution))

; Test
;(printf "~s/n" (Unsat P1 (list (list (cons 4 'geqz)) (list (cons 4 'leqz)) (list (cons 4 'eqz))) R))