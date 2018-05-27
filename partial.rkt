#lang racket
(provide (all-defined-out))

(define-struct/contract Partial ([ID integer?]
                                 [Non-Terminal any/c]
                                 [Terminal any/c]
                                 [Filled? boolean?]
                                 [Children list?])
  #:property prop:sequence
  (lambda (P)
    (define (Partial->List P)
      (cons P
      (apply
       append
       (for/list ([child (Partial-Children P)]) (Partial->List child)))))
    (in-list (Partial->List P))))

(define (Production-Children p)
  (match p
    [`(,n ,t ...) t]
    [else '()]))

(define (Production-Terminal p)
  (match p
    [`(,n ,t ...) n]
    [else p]))

(define (Partial-Size P)
    (sequence-count identity P))

(define (Hole? P) (not (Partial-Filled? P)))
(define (Holes P) (for/list ([node P] #:when (Hole? node)) (Partial-ID node)))

(define (Lookup-By-ID P H)
  (for/or ([node P] #:when (eq? (Partial-ID node) H)) node))

(define (Fill P H p)
  (define next (Partial-Size P))
  (define (Search P)
    (if (= (Partial-ID P) H)
        (Partial (Partial-ID P) (Partial-Non-Terminal P) (Production-Terminal p) #t
                 (for/list ([new (Production-Children p)])
                   (begin0
                     (Partial next new 'HOLE #f '())
                     (set! next (+ next 1)))))
        (Partial (Partial-ID P) (Partial-Non-Terminal P) (Partial-Terminal P) (Partial-Filled? P)
                 (for/list ([child (Partial-Children P)])
                   (Search child)))))
  (Search P))

(define P1 (Partial 0 'N 'head #t
                    (list
                     (Partial 1 'L 'take #t
                              (list
                               (Partial 2 'L 'filter #t
                                        (list
                                         (Partial 3 'L 'x1 #t '())
                                         (Partial 4 'T 'HOLE #f '())))
                               (Partial 5 'N 'HOLE #f '()))))))

(define P3 (Partial 0 'N 'head #t
                    (list
                     (Partial 1 'L 'take #t
                              (list
                               (Partial 2 'L 'filter #t
                                        (list
                                         (Partial 3 'L 'x1 #t '())
                                         (Partial 4 'T 'HOLE #f '())))
                               (Partial 5 'N 'maximum #t (list (Partial 6 'L 'HOLE #f '()))))))))
(define (print-node P)
    (printf "~s: ~s, ~s, ~s\n" (Partial-ID P) (Partial-Non-Terminal P) (Partial-Terminal P) (Partial-Filled? P)))

(define (print-partial P)
  (printf "Partial-Tree: \n")
  (define (print-tree P level)
    (for ([i (in-range 0 level)])
      (printf "\t"))
    (print-node P)
    (for ([child (Partial-Children P)])
      (print-tree child (+ level 1))))
  (print-tree P 1)
  (newline))

(define (Make-prod->unique rules)
  (define i 0)
  (define prods->unique
    (for*/hash ([(non-term terms) rules] [term terms])
      (begin0
        (values (list `,(Production-Terminal term)) i)
        (set! i (+ i 1)))))
  prods->unique)

(define (Prod->Unique prod->unique prod)
  (dict-ref prod->unique (list prod)))

(define (Node->Unique num-prods node) (* (Partial-ID node) num-prods))
