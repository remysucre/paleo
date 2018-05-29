#lang racket

(require rackunit "smt.rkt" "partial.rkt")

(provide (all-defined-out))

; CHECKCONFLICT

; procedure CheckConflict(P, Ψ, Φ)
;  ΦP ← InferSpec(P)
;  ψ  ← SMTSolve(ΦP ∧ Φ)
;  κ  ← {(φ, N, χN) | φ ∈ ψ ∧ N = Node(φ)}
;  κ' ← {(φ',N, χN) | φ'= Rename(φ) ∧ (φ, N, χN) ∈ κ}
;  return k'

; returns the MUC of a conflict
(define (CheckConflict P Psi Phi)
  (print 'checkcstart)
  (print-partial P)
  (define (phi-n-xn muc)
    (let* ([id (string->number (substring (symbol->string muc) 1))]
           [xn (Partial-Terminal (Lookup-By-ID P id))]
           [ph (sem xn Psi)])
      (list ph id xn)))
  (define (declare-xs phi)
    (cons '(declare-const y (List Int))
          (map (lambda (x) (list 'declare-const x '(List Int)))
               (filter (lambda (c) (string-prefix? (symbol->string c) "x")) (set->list (list->set (filter symbol? (flatten phi))))))))
  (let* ([Phi-P (InferSpec P Psi)]
         [encoding (append (declare-xs Phi) Phi-P (list (list 'assert (list '! Phi ':named 'aphi))))]
         [psi0 (SMTSolve encoding)]
         [psi (if (list? psi0) psi0 '())]
         [k (map phi-n-xn (filter (lambda (x) (not (equal? x 'aphi)))psi))])
    (print k) k))

(define (InferSpec p Psi)
  (define (declare-vs p)
    (for/list ([n p])
        (list 'declare-const (v_s (Partial-ID n)) '(List Int))))
  (define (phi-p p Psi)
    (subst (v_s (Partial-ID p)) 'y (phi-n p Psi)))
  (define (phi-n p Psi)
    (for/list ([n p])
      (list 'assert (list '! (psi-n n Psi) ':named
                          (string->symbol (string-append "a" (number->string (Partial-ID n))))))))
  (define (psi-n p Psi)
    (if (not (Partial-Filled? p))
        'true
        (if (null? (Partial-Children p))
            (if (number? (Partial-Terminal p))
                (list '= (v_s (Partial-ID p)) (list 'insert (Partial-Terminal p) 'nil))
                (list '= (v_s (Partial-ID p)) (Partial-Terminal p)))
            (substs (xs-of (sem (Partial-Terminal p) Psi))
                    (C (Partial-Children p))
                    (subst 'y (v_s (Partial-ID p)) (sem (Partial-Terminal p) Psi))))))
  (define (xs-of s) (set-intersect (flatten s) '(x1 x2))) ; do this properly
  (define (C cs) (map (lambda (n) (v_s (Partial-ID n))) cs))
  (define (v_s id) (string->symbol (string-append "v" (number->string id))))
  (let ([vs (declare-vs p)]
        [phi-ns (phi-p p Psi)]) (append vs phi-ns)))

(define (sem p Psi)
  (print p)
  (print (number? p))
  (dict-ref Psi p
            (if (number? p)
                (list '= 'y (list 'insert p 'nil))
                (list '= 'y p))))

; ANALYZECONFLICT


; Partial-Tree: 

; 1: N, 0, #t
; '(#hash((N . (0 1 2 3 4 5 6 7 8 9 10 x1 x2 x3 x4 x5 (last L) (head L) (sum L) (maximum L) (minimum L))) (T . (geqz leqz eqz)) (L . ((take L N) (filter L T) (sort L) (reverse L))))
; #hash((minimum . (and (> (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (= (min x1) (min y)))) (sum . (and (>= (len x1) 1) (= (len y) 1))) (reverse . (and (= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y)) (= (first x1) (last y)) (= (last x1) (first y)))) (maximum . (and (> (len x1) 1) (= (len y) 1) (= (max x1) (max y)) (<= (min x1) (min y)))) (take . (and (< (len y) (len x1)) (>= (max x1) (max y)) (<= (min x1) (min y)) (> (head x2) 0) (> (len x1) (head x2)) (= (first x1) (first y)))) (last . (and (>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (last x1)) (= (last y) (last x1)))) (filter . (and (< (len y) (len x1)) (= x2 x2) (>= (max x1) (max y)) (<= (min x1) (min y)))) (0 . (= y 0)) (sort . (and (= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y)))) (head . (and (>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (first x1)) (= (last y) (first x1)))))
; (((= y 0) 1 0)))

; learn lemmas from the MUC of a conflict
(define (AnalyzeConflict P gamma Psi kappa)
  (print 'begin)
  (print-partial P)
  (print (list gamma Psi kappa))
  (print 'end)
    (for/fold ([sphi '()]) ([clause kappa])
      (match-define (list phi node p0) clause)
      (define As (Partial-Children (Lookup-By-ID P node)))
      (define rules gamma)
      #;(define sigma (for*/list ([A (cons (Lookup-By-ID P node) As)]
                                [prod (dict-ref rules (Partial-Non-Terminal A))]
                                #:when (list? prod)
                                #:when (list? (SMTSolve `((declare-const x1 (List Int))
                                                          (declare-const x2 (List Int))
                                                          (declare-const y (List Int))
                                                          (assert
                                                           (and ,(sem (Production-Terminal prod) Psi)
                                                                (not ,phi)))))))
                      (list (Partial-ID A) (Production-Terminal prod))))
      (append sphi (list (list (cons node p0))))))

; EXAMPLES

;N → x_i |-3|-2|-1|0|1|2|3
;  | last(L) | head(L) | access(L, N )
;  | minimum(L) | maximum(L) | sum(L) | count(L, T , N )
;L → x_i | take(L,N) | drop(L,N)
;  | sort(L) | reverse(L) | filter(L, T , N )
;  | map(L,F,N) | zipWith(L,L,Z) | scanl1(L,Z)
;T → geq | leq | eq | neq | modeq | modneq
;F → mul | div | plus | minus | pow
;Z → plus | minus | mul | min | max

; grammar
(define R1
  (make-immutable-hash
   (list (cons 'N (append `,(range 0 5) '(x1 x2) '((last L) (head L) (sum L) (maximum L) (minimum L))))
         (cons 'L (append '(x1 x2) (list '(take L N) '(filter L T) '(sort L) '(reverse L))))
         (cons 'T '(geqz leqz eqz)))))

; semantics
(define Psi0 #hash((filter . (and (< (len y) (len x1)) (= x2 x2) (>= (max x1) (max y)) (<= (min x1) (min y))))
                  (minimum . (and (> (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (= (min x1) (min y))))
                  (maximum . (and (> (len x1) 1) (= (len y) 1) (= (max x1) (max y)) (<= (min x1) (min y))))
                  (last . (and (>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (last x1)) (= (last y) (last x1))))
                  (head . (and (>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (first x1)) (= (last y) (first x1))))
                  (sum . (and (>= (len x1) 1) (= (len y) 1)))
                  (take . (and (< (len y) (len x1)) (>= (max x1) (max y)) (<= (min x1) (min y)) (> (head x2) 0) (> (len x1) (head x2)) (= (first x1) (first y))))
                  (reverse . (and (= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y)) (= (first x1) (last y)) (= (last x1) (first y))))
                  (sort . (and (= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y))))))

(define Psi #hash((filter . (and (< (len y) (len x1)) (>= (max x1) (max y)) (<= (min x1) (min y))))
                  (minimum . (and (> (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (= (min x1) (min y))))
                  (maximum . (and (> (len x1) 1) (= (len y) 1) (= (max x1) (max y)) (<= (min x1) (min y))))
                  (last . (and (>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (last x1)) (= (last y) (last x1))))
                  (head . (and (>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (first x1)) (= (last y) (first x1))))
                  (sum . (and (>= (len x1) 1) (= (len y) 1)))
                  (take . (and (< (len y) (len x1)) (>= (max x1) (max y)) (<= (min x1) (min y)) (> (head x2) 0) (> (len x1) (head x2)) (= (first x1) (first y))))
                  (reverse . (and (= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y)) (= (first x1) (last y)) (= (last x1) (first y))))
                  (sort . (and (= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y))))))

(sem 0 Psi)