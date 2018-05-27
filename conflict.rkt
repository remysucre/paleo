#lang racket

(require rackunit "smt.rkt" "partial.rkt")

(provide (all-defined-out))

; procedure CheckConflict(P, Ψ, Φ)
;  ΦP ← InferSpec(P)
;  ψ  ← SMTSolve(ΦP ∧ Φ)
;  κ  ← {(φ, N, χN) | φ ∈ ψ ∧ N = Node(φ)}
;  κ' ← {(φ',N, χN) | φ'= Rename(φ) ∧ (φ, N, χN) ∈ κ}
;  return k'

(define (declare-v id) (list 'declare-const (string->symbol (string-append "v" (number->string id))) '(List Int)))

(define (declare-vs p)
  (for/list ([n p]) (declare-v (Partial-ID n))))

  ; [((list id A p)) (list (declare-v id))]
  ; [((list id A p cs)) (flatten (cons (declare-v id) (map declare-vs cs)))])

;(define/match (declare-vs p Psi)
;  [(list id A p cs) (cons Psi-of p Psi)]
;  [(list id A 'HOLE)])

(define (v_s id) (string->symbol (string-append "v" (number->string id))))
(define (sem p Psi) (dict-ref Psi p))

#;(define Psi0 #hash((last    . (and (= x1 x1) (= y y)));"Lin.len ≥ 1 ∧ Lout .len = 1 ∧ Lin.max ≥ Lout.max ∧ Lin.min ≤ Lout.min ∧ Lout.first = Lin.last ∧ Lout.last = Lin.last")
                   (head    . (and (= x1 x1) (= y y)));"Lin.len ≥ 1 ∧ Lout .len = 1 ∧ Lin.max ≥ Lout.max ∧ Lin.min ≤ Lout.min ∧ Lout.first = Lin.first ∧ Lout.last = Lin.first")
                   (sum     . (and (= x1 x1) (= y y)));"Lin.len ≥ 1 ∧ Lout .len = 1")
                   (maximum . (and (= x1 x1) (= y y)));"Lin.len > 1 ∧ Lout.len = 1 ∧ Lin.max = Lout.max ∧ Lout.min ≥ Lin.min")
                   (minimum . (and (= x1 x1) (= y y)));"Lin.len > 1 ∧ Lout.len = 1 ∧ Lin.max ≥ Lout.max ∧ Lout.min = Lin.min")
                   (take    . (and (= x1 x1) (and (= y y) (= x2 x2))));"Lout.len < Lin.len Lin.max ≥ Lout.max Lin.min ≤ Lout.min k > 0 ∧ Lin.len > k Lin.first = Lout.first")
                   (filter  . (and (= x1 x1) (and (= y y) (= x2 x2))));"Lout.len < Lout.len Lout.max ≤ Lin.max Lout.min ≥ Lin.min")
                   (sort    . (and (= x1 x1) (= y y)));"Lout.len = Lin.len > 1 ∧ Lin.max = Lout.max ∧ Lin.min = Lout .min")
                   (reverse . (and (= x1 x1) (= y y)))));"Lout.len = Lin.len > 1 ∧ Lin.max = Lout.max ∧ Lin.min = Lout.min ∧ Lin.first = Lout.last ∧ Lin.last = Lout.first")))

#;p(define Psi1 #hash((last   . "Lin.len ≥ 1 ∧ Lout .len = 1 ∧ Lin.max ≥ Lout.max ∧ Lin.min ≤ Lout.min ∧ Lout.first = Lin.last ∧ Lout.last = Lin.last")
                  (head    . "Lin.len ≥ 1 ∧ Lout .len = 1 ∧ Lin.max ≥ Lout.max ∧ Lin.min ≤ Lout.min ∧ Lout.first = Lin.first ∧ Lout.last = Lin.first")
                  (sum     . "Lin.len ≥ 1 ∧ Lout .len = 1")
                  (maximum . "Lin.len > 1 ∧ Lout.len = 1 ∧ Lin.max = Lout.max ∧ Lout.min ≥ Lin.min")
                  (minimum . "Lin.len > 1 ∧ Lout.len = 1 ∧ Lin.max ≥ Lout.max ∧ Lout.min = Lin.min")
                  (take    . "Lout.len < Lin.len Lin.max ≥ Lout.max Lin.min ≤ Lout.min k > 0 ∧ Lin.len > k Lin.first = Lout.first")
                  (filter  . "Lout.len < Lout.len Lout.max ≤ Lin.max Lout.min ≥ Lin.min")
                  (sort    . "Lout.len = Lin.len > 1 ∧ Lin.max = Lout.max ∧ Lin.min = Lout .min")
                  (reverse . "Lout.len = Lin.len > 1 ∧ Lin.max = Lout.max ∧ Lin.min = Lout.min ∧ Lin.first = Lout.last ∧ Lin.last = Lout.first")))

(define Psi0 #hash((filter . ((< (len y) (len x1)) (>= (max x1) (max y)) (<= (min x1) (min y))))
                  (minimum . ((> (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (= (min x1) (min y))))
                  (maximum . ((> (len x1) 1) (= (len y) 1) (= (max x1) (max y)) (<= (min x1) (min y))))
                  (last . ((>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (last x1)) (= (last y) (last x1))))
                  (head . ((>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (first x1)) (= (last y) (first x1))))
                  (sum . ((>= (len x1) 1) (= (len y) 1)))
                  (take . ((< (len y) (len x1)) (>= (max x1) (max y)) (<= (min x1) (min y)) (> (head x2) 0) (> (len x1) (head x2)) (= (first x1) (first y))))
                  (reverse . ((= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y)) (= (first x1) (last y)) (= (last x1) (first y))))
                  (sort . ((= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y))))))

(sem 'last Psi0)

(define (C cs) (map (lambda (n) (v_s (Partial-ID n))) cs))



(define (xs-of s) (set-intersect (flatten s) '(x1 x2))) ; do this properly

(xs-of '(Lout \. len = Lin \. len > 1 ∧ Lin \. max = Lout \. max ∧ Lin \. min = Lout \. min ∧ Lin \. first = Lout \. last ∧ Lin \. last = Lout \. first))

(define (phi-p p Psi) (list 'assert (subst 'y (v_s (Partial-ID p)) (phi-n p Psi))))

(define (psi-n p Psi)
  (if (not (Partial-Filled? p))
      'true
      (if (null? (Partial-Children p))
          'true ; TODO careful (subst (v_s (Partial-ID p)) 'y (sem (Partial-Terminal p) Psi))
          (substs (xs-of (sem (Partial-Terminal p) Psi)) (C (Partial-Children p)) (subst 'y (v_s (Partial-ID p)) (sem (Partial-Terminal p) Psi))))))

(define (phi-n p Psi)
  (foldr conj 'true (cons (psi-n p Psi) (map (lambda (c) (phi-n c Psi)) (Partial-Children p)))))

(Partial-Children P1)



(define (InferSpec p Psi)
  (let ([vs (declare-vs p)]
        [phi-ns (phi-p p Psi)]) (append vs (list phi-ns))))

(InferSpec P1 Psi0)

; returns the MUC of a conflict
(define (CheckConflict P Psi Phi)
  (define (Chi n) (Partial-Terminal (Lookup-By-ID P n)))
  (let* ([Phi-P (InferSpec P Psi)]
         [psi0 (SMTSolve (append Phi-P (list (list 'assert Phi))))]
         [psi (if (list? psi0) psi0 '())]
         [k (map (lambda (phi) (list phi (Node phi) (Chi (Node phi)))) psi)]
         [k_ (map (match-lambda [(list phi N X) (list (Rename phi) (Node phi) (Chi (Node phi)))]) k)])
    k_))

; learn lemmas from the MUC of a conflict
#;(define (AnalyzeConflict P gamma Psi kappa)
  (define (step phi x)
    (match x [(list ph N X)
              (match-let* ([As (map (lambda (Ni) (Aof Ni)) (Children N))]
                           [sig (filter (lambda (x) (implies (Of Psi x) ph)) (sig-of As))])
                (disj ph (foldr conj #t (map (lambda (x) (neg (assigned-to N x))) sig))))]))
  (foldr (lambda (ki phi) step phi ki) #f kappa))

(define R1 (make-immutable-hash
           (list (cons 'N (append `,(range 0 11) '(x1 x2 x3 x4 x5) '((last L) (head L) (sum L) (maximum L) (minimum L))))
             (cons 'L (list '(take L N) '(filter L T) '(sort L) '(reverse L)))
             (cons 'T '(geqz leqz eqz)))))

(define Psi #hash((filter . ((< (len y) (len x1)) (>= (max x1) (max y)) (<= (min x1) (min y))))
                  (minimum . ((> (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (= (min x1) (min y))))
                  (maximum . ((> (len x1) 1) (= (len y) 1) (= (max x1) (max y)) (<= (min x1) (min y))))
                  (last . ((>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (last x1)) (= (last y) (last x1))))
                  (head . ((>= (len x1) 1) (= (len y) 1) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (first x1)) (= (last y) (first x1))))
                  (sum . ((>= (len x1) 1) (= (len y) 1)))
                  (take . ((< (len y) (len x1)) (>= (max x1) (max y)) (<= (min x1) (min y)) (> (head x2) 0) (> (len x1) (head x2)) (= (first x1) (first y))))
                  (reverse . ((= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y)) (= (first x1) (last y)) (= (last x1) (first y))))
                  (sort . ((= (len y) (len x1)) (> (len x1) 1) (= (max x1) (max y)) (= (min x1) (min y))))))
  
(define (AnalyzeConflict P gamma Psi kappa)
    (for/fold ([sphi '()]) ([clause kappa])
      (match-define (list phi node prod) clause)
      (define As (Partial-Children (Lookup-By-ID P node)))
      ;(define rules (caddr gamma))
      (define rules gamma)
      (define sigma (for*/list ([node As]
                                [prod (dict-ref rules (Partial-Non-Terminal node))]
                                #:when (list? prod)
                                #:when (not (SMTSolve `((assert
                                                    (and ,@(dict-ref Psi (Production-Terminal prod))
                                                         (not ,phi)))))))
                      (list (Partial-ID node) (Production-Terminal prod))))
      (append sphi sigma)))

;Test
#;(AnalyzeConflict P1 R1 Psi '(((= (len y) 1) 1 maximum)))

(define (Aof x) '()) ; 
(define (Children x) '()) ; TODO
(define (Of x) '()) ; TODO
(define (sig-of x) '()) ; TODO
(define (disj x) '()) ; TODO
(define (neg x) '()) ; TODO
(define (assigned-to x) '()) ; TODO

(define P2 '(0 N head
                ((1 L take
                    ((3 L filter ((7 L x1)
                                  (8 T HOLE)))
                     (4 N HOLE))))))

; (declare-vs P2)

; (Backtrack ((disj (neg c 0 filter) (neg c 2 eqz)) (disj (neg c 0 filter) (neg c 2 leqz)))) (0 1) (2 2) (0 p0) (1 P2) (2 p2)
; lookup 2 pps

(define (d2l d) (dict-map d (lambda (x y) (cons x y))))

(define (revert l xs) (make-immutable-hash (filter (lambda (x) (< (car x) l)) (d2l xs))))

(revert 3 (list (cons 1 2) (cons 2 4) (cons 4 5)))

; takes in a conflict, the decision history and the partial progam history
; returns the partial program at the second highest decision level in the conflict
(define (backtrack omega ds pps)
  (let* ([snd-l (cadadr (sort-with omega ds))]
        [pp (dict-ref pps snd-l)]
        [pps_ (revert snd-l pps)]
        [ds_ (revert snd-l ds)])
    (list pp pps_ ds_)))

; backtrack finds the second highest decision level in the MUC omega according to ds
; then returns the partial program at that level from pps
; as well as reverting ds and pps to that level

; a partial program

; pairs of decision level and partial program

(define pps-eg (list (cons 1 P2)))

; pairs of decision level and node id
(define ds-eg #hash((1 . 2) (0 . 3) (7 . 1) (3 . 5)))

; sort the conflict with decision level

(define (sort-with omega ds)
  (define (less x y) (< (dict-ref ds (cadr x)) (dict-ref ds (cadr y))))
  (sort omega less))

; unsat core with decision
(define om-eg '(((leq y (max x1)) 0 'head)
                ((leq ((max y) (max x1))) 1 'take)
                ((leq (max y) (max x1)) 3 'filter)
                ((= y x1) 7 'x1)))

(backtrack om-eg ds-eg pps-eg)

(declare-vs P1)

(CheckConflict P1 Psi0 'false)