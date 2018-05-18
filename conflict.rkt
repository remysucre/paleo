#lang racket

(provide (all-defined-out))

; TODO long

; procedure CheckConflict(P, Ψ, Φ)
;  ΦP ← InferSpec(P)
;  ψ  ← SMTSolve(ΦP ∧ Φ)
;  κ  ← {(φ, N, χN) | φ ∈ ψ ∧ N = Node(φ)}
;  κ' ← {(φ',N, χN) | φ'= Rename(φ) ∧ (φ, N, χN) ∈ κ}
;  return k'

(define (conj x y) '()) ; TODO 
(define (InferSpec p) '()) ; TODO
(define (SMTSolve f) '()) ; TODO
(define (Node f) '()) ; TODO
(define (Chi n) '()) ; TODO
(define (Rename p) '()) ; TODO

; returns the MUC of a conflict
(define (CheckConflict P Psi Phi)
  (let* ([Psi-P (InferSpec P)]
         [psi (SMTSolve (conj Psi-P Psi))]
         [k (map (lambda (phi) (list phi (Node phi) (Chi (Node phi)))) psi)]
         [k_ (map (match-lambda [(list phi N X) (list (Rename phi) (Node phi) (Chi (Node phi)))]) k)])
    k_))

; learn lemmas from the MUC of a conflict
(define (AnalyzeConflict P gamma Psi kappa)
  (define (step phi x)
    (match x [(list ph N X)
              (match-let* ([As (map (lambda (Ni) (Aof Ni)) (Children N))]
                           [sig (filter (lambda (x) (implies (Of Psi x) ph)) (sig-of As))])
                (disj ph (foldr conj #t (map (lambda (x) (neg (assigned-to N x))) sig))))]))
  (foldr (lambda (ki phi) step phi ki) #f kappa))

(define (Aof x) '()) ; TODO

(define (Children x) '()) ; TODO

(define (Of x) '()) ; TODO
(define (sig-of x) '()) ; TODO
(define (disj x) '()) ; TODO
(define (neg x) '()) ; TODO
(define (assigned-to x) '()) ; TODO

(define P1 '(0 N head
                (1 L take
                    ((3 L filter ((7 L x1)
                                  (8 T HOLE)))
                     (4 N HOLE)))))

; (Backtrack ((disj (neg c 0 filter) (neg c 2 eqz)) (disj (neg c 0 filter) (neg c 2 leqz)))) (0 1) (2 2) (0 p0) (1 p1) (2 p2)
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

(define pps-eg (list (cons 1 P1)))

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