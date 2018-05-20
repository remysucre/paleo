#lang racket
(provide (all-defined-out))

(define-struct/contract Partial ([ID integer?]
                                 [Non-Terminal any/c]
                                 [Terminal any/c]
                                 [Filled? boolean?]
                                 [Children list?])
  #:property prop:sequence
  (lambda (P)
    (in-list (Partial-Children P))))
(define-struct/contract Partial-Tree ([Root Partial?] [NextID integer?]))

(define (Production-Children p)
  (match p
    [`(,n ,t ...) t]
    [else '()]))

(define (Production-Terminal p)
  (match p
    [`(,n ,t ...) n]
    [else p]))

(define (Partial-Size P)
    (+ 1
       (apply + 
              (for/list ([child (Partial-Children P)])
                (Partial-Size child)))))

(define/contract (Make-Partial-Tree root)
  (-> Partial? Partial-Tree?)
  (Partial-Tree root (Partial-Size root)))

(define (Hole? P) (not (Partial-Filled? P)))
(define (Holes P)
  (define (Search P)
    (if (Hole? P)
        (Partial-ID P)
        (for/fold ([defualt '()]) ([child (Partial-Children P)])
          (define below (Search child))
          (cond
            [(null? below) defualt]
            [(null? defualt) below]
            [else (cons below defualt)]))))
  (define (Pairs->List pairs)
    (if (pair? pairs)
        (cons (car pairs) (Pairs->List (cdr pairs)))
        (cons pairs '())))
  (Pairs->List (Search (Partial-Tree-Root P))))

(define (Lookup-By-ID P H)
  (define (lookup P H)
    (if (= (Partial-ID P) H)
        P
        (for/or ([child (Partial-Children P)])
          (lookup child H))))
  (lookup (Partial-Tree-Root P) H))

(define (Fill P H p)
  (define next (Partial-Tree-NextID P))
  (define (Search P)
    (if (= (Partial-ID P) H)
        (Partial (Partial-ID P) (Partial-Non-Terminal P) (Production-Terminal p) #t
                 (for/list ([new (Production-Children p)])
                   (Partial next new 'HOLE #f '())))
        (Partial (Partial-ID P) (Partial-Non-Terminal P) (Partial-Terminal P) (Partial-Filled? P)
                 (for/list ([child (Partial-Children P)])
                   (Search child)))))
  (Make-Partial-Tree (Search (Partial-Tree-Root P))))

(define P1 (Partial 0 'N 'head #t
                    (list
                     (Partial 1 'L 'take #t
                              (list
                               (Partial 2 'L 'filter #t
                                        (list
                                         (Partial 3 'L 'x1 #t '())
                                         (Partial 4 'T 'HOLE #f '())))
                               (Partial 5 'N 'HOLE #f '()))))))

(define (print-partial P)
  (define root (Partial-Tree-Root P))
  (printf "Partial-Tree: \n")
  (define (print-tree P level)
    (for ([i (in-range 0 level)])
      (printf "\t"))
    (printf "~s: ~s, ~s, ~s\n" (Partial-ID P) (Partial-Non-Terminal P) (Partial-Terminal P) (Partial-Filled? P))
    (for ([child (Partial-Children P)])
      (print-tree child (+ level 1))))
  (print-tree root 1)
  (newline))

#;(define (pi P rules)
  (for/list ([node P])
    `(=,(string->symbol (format "c~s-~s\n" ) ,(if ())))))