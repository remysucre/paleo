#lang racket

(provide (all-defined-out))
(require "partial.rkt" "smt.rkt" "conflict.rkt" racket/random)

; TODO long
#;(define (Decide P gamma phi Omega)
  '(1 2))

; TODO probably pretty short enumeration over the tree
(define (Consistent? P omega rules)
  (define referenced (make-hash))
  (define (reference name)
    (unless (hash-has-key? referenced name)
      (dict-set! referenced name #t)))
  
  (define pi
    (if (Hole? P)
        `(assert true)
        `(assert (and
                  ,@(for/list ([node P] #:when (Partial-Filled? node))
                      (reference (ToZ3Name node (Partial-Terminal node)))
                      `,(ToZ3Name node (Partial-Terminal node)))))))

  (define conflicts
    (if (null? omega)
        '(assert true)
        `(assert (and
                  ,@(for/list ([kappa omega])
                      `(or
                        ,@(for/list ([clause kappa])
                            `(and
                              ,@(for/list ([literal clause])
                                  (define prod (cdr literal))
                                  (reference (Prod->ToZ3Name (car literal) prod))
                                  `(not ,(Prod->ToZ3Name (car literal) prod)))))))))))

  (define decs
    (for/list ([name (in-dict-keys referenced)])
      `(declare-const ,name Bool)))

  (define solution (SATSolve (append decs (list pi conflicts))))
  (not (list? solution)))

(define (L in out x)
  0
  #;(cond
    [(= (length in) 1)
     (random 1 5)]
    [(= (length in) (length out))
     (if (= (car in) (car out))
         (match x
           ['sort (random 1 10)]
           ['reverse (random 1 10)]
           ['x1 (random 5 15)]
           ['take (random 1 5)]
           [(? number?) (random 1 5)]
           [else 0])
         (match x
           ['sort (random 1 5)]
           ['reverse (random 1 5)]
           ['x1 (random 1 10)]
           ['take (random 1 10)]
           [(? number?) (random 1 10)]
           [else 0]))]
    [(= 1 (length out))
     (match x
       ['sort (if (= (car in) (car out)) (random 1 5) (random 1 10))]
       ['reverse (if (= (car in) (car out)) (random 1 5) (random 1 10))]
       ['minimum (if (equal? (Minimum in) (Minimum out)) (random 5 15) (random 1 3))]
       ['maximum (if (equal? (Maximum in) (Maximum out)) (random 5 15) (random 1 3))]
       ['head (if (= (car in) (car out)) (random 1 10) (random 1 5))]
       ['last (if (= (car in) (car out)) (random 1 5) (random 1 10))]
       ['sum (if (> (car (Maximum out)) (car (Maximum in))) (random 3 17) (random 1 5))]
       ['x1 (random 3 10)]
       ['take (random 1 10)]
       [1 (random 1 10)]
       [(? number?) (random 1 7)]
       [else 0])]
    [(< 1 (length out) (length in))
     (match x
       ['sort (if (= (car in) (car out)) (random 1 5) (random 10))]
       ['reverse (if (= (car in) (car out)) (random 1 5) (random 10))]
       ['x1 (random 3 13)]
       ['take (random 3 13)]
       [1 (random 1 5)]
       [(? number?) (random 3 13)]
       [else 0])]
    [else 1]))

(define R (make-immutable-hash
           (list (cons 'N (append `,(range 0 11) '(x1 x2 x3 x4 x5) '((last L) (head L) (sum L) (maximum L) (minimum L))))
             (cons 'L (list '(take L N) '(filter L T) '(sort L) '(reverse L)))
             (cons 'T '(geqz leqz eqz)))))

; TODO make sure this matches data structure
(define (Decide P gamma in out Omega rules)
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
 (argmax (lambda (x) (L in out x)) (apply append V1)))

;Test
; (eprintf "~s\n" (Decide P1 #f #f (list (list (cons 4 'geqz) (cons 2 'filter)) (list (cons 4 'leqz) (cons 2 'filter))) R))

; (Decide P1 #f #f (list (list (cons 4 'geqz) (cons 2 'filter)) (list (cons 4 'leqz) (cons 2 'filter))) R)

; TODO long
(define (Implied P H p omega rules)
  (define referenced (make-hash))
  (define (reference name)
    (unless (hash-has-key? referenced name)
      (dict-set! referenced name #t)))
  
  (define pi
    (if (Hole? P)
        `(assert true)
        `(assert (and
                  ,@(for/list ([node P] #:when (Partial-Filled? node))
                      (reference (ToZ3Name node (Partial-Terminal node)))
                      `,(ToZ3Name node (Partial-Terminal node)))))))

  (define conflicts
    (if (null? omega)
        '(assert true)
        `(assert (and
                  ,@(for/list ([kappa omega])
                      `(or
                        ,@(for/list ([clause kappa])
                            `(and
                              ,@(for/list ([literal clause])
                                  (define prod (cdr literal))
                                  (reference (Prod->ToZ3Name (car literal) prod))
                                  `(not ,(Prod->ToZ3Name (car literal) prod)))))))))))

  (define h-node (Lookup-By-ID P H))
  
  (define possible
    `(assert (and
      ,@(for/list ([(prod-non prod-terms) (in-dict rules)]
                   #:when (eq? (Partial-Non-Terminal h-node) prod-non))
          `(or
            ,@(for/list ([prod-term prod-terms])
                (reference (ToZ3Name h-node (Production-Terminal prod-term)))
                `,(ToZ3Name h-node (Production-Terminal prod-term))))))))

  (reference (ToZ3Name h-node (Production-Terminal p)))
  (define invert `(assert (not ,(ToZ3Name h-node (Production-Terminal p)))))

  (define decs
    (for/list ([name (in-dict-keys referenced)])
      `(declare-const ,name Bool)))

  (define solution (SATSolve (append decs (cons invert (append (list pi conflicts possible))))))
  (list? solution))

(define (Propagate P gamma H p Omega rules)
  (define P1 (Fill P H p))
  (define S (for*/list ([hole (Holes P1)]
                        #:when (number? hole)
                        [(prod-non prod-terms) (in-dict rules)]
                        #:when (eq? (Partial-Non-Terminal (Lookup-By-ID P1 hole)) prod-non)
                        [prod-term prod-terms])
              (list hole prod-term)))
  (for/fold ([default P1]) ([x S])
    (match-define (list hole prod) x)
    (define hole-non (Partial-Non-Terminal (Lookup-By-ID P1 hole)))
    (if (Implied P1 hole prod Omega rules)
       (Propagate P1 gamma hole prod Omega rules)
        default)))

; Test
;(print-partial (Propagate P1 #f 5 0 (list (list (cons 4 'geqz) (cons 2 'filter)) (list (cons 4 'leqz) (cons 2 'filter))) R))

; TODO long
(define (Unsat P omega rules)
  (define referenced (make-hash))
  (define (reference name)
    (unless (hash-has-key? referenced name)
      (dict-set! referenced name #t)))
  
  (define pi
    (if (Hole? P)
        `(assert true)
        `(assert (and
                  ,@(for/list ([node P] #:when (Partial-Filled? node))
                      (reference (ToZ3Name node (Partial-Terminal node)))
                      `,(ToZ3Name node (Partial-Terminal node)))))))

  (define conflicts
    (if (null? omega)
        '(assert true)
        `(assert (and
                  ,@(for/list ([kappa omega])
                      `(or
                        ,@(for/list ([clause kappa])
                            `(and
                              ,@(for/list ([literal clause])
                                  (define prod (cdr literal))
                                  (reference (Prod->ToZ3Name (car literal) prod))
                                  `(not ,(Prod->ToZ3Name (car literal) prod)))))))))))

  (define possible
    (if (null? (Holes P))
        `(assert true)
        `(assert (and
                  ,@(for*/list ([node P]
                                #:when (Hole? node)
                                [(prod-non prod-terms) (in-dict rules)]
                                #:when (eq? (Partial-Non-Terminal node) prod-non))
                      `(or
                        ,@(for/list ([prod-term prod-terms])
                            (reference (ToZ3Name node (Production-Terminal prod-term)))
                            `,(ToZ3Name node (Production-Terminal prod-term)))))))))
  
  (define decs
    (for/list ([name (in-dict-keys referenced)])
      `(declare-const ,name Bool)))

  (define solution (SATSolve (append decs (list pi possible conflicts))))
  (list? solution))

; Test
;(printf "~s\n" (Unsat P1 (list (list (cons 4 'geqz) (cons 4 'leqz) (cons 4 'eqz))) R))
;(printf "~s\n" (Unsat P1 (list (list (cons 4 'geqz) (cons 4 'leqz))) R))
;(printf "~s\n" (Unsat P1 (list (list (cons 4 'geqz) (cons 4 'leqz))) R))