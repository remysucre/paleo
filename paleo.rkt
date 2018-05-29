#lang racket
(require rackunit "conflict.rkt" "decide.rkt" "partial.rkt")

; BACKTRACK
; 
; backtrack finds the second highest decision level in the MUC omega according to ds
; then returns the partial program at that level from pps
; as well as reverting ds and pps to that level

; takes in a conflict, the decision history and the partial progam history
; returns the partial program at the second highest decision level in the conflict
(define (backtrack omega ds pps)
  (print omega)
  (print ds)
  (print pps)
  (define (revert l xs) (filter (lambda (x) (<= (car x) l)) xs))
  (define (less x y) (< (dict-ref ds (caar x)) (dict-ref ds (caar y))))
  (let* ([snd-l (if (< (length omega) 2) 0 (dict-ref ds (caaadr (sort omega less))))]
         [pp (dict-ref pps snd-l)]
         [pps_ (revert snd-l pps)]
         [ds_ (if (= 0 snd-l) '() (revert snd-l ds))])
    (list pp pps_ ds_ snd-l)))

; procedure Synthesize(G, Ψ, Φ)
;   P ← Root(S)
;   Ω ← ∅
;   while true do
;     (H,p) ← Decide(P, G, Φ, Ω)
;     P ← Propagate(P, G, (H,p), Ω)
;     κ ← CheckConflict(P, Ψ, Φ)
;     if κ ≠ ∅ then
;        Ω ← Ω ∪ AnalyzeConflict(P, G, Ψ, κ)
;        P ← Backtrack(P, Ω)
;     if UNSAT(∧_{φ∈Ω} φ) then
;        return ⊥
;     else if IsConcrete(P) then
;        return P
(define (synth gamma Psi Phi)
  (define P0 (Partial 1 'N 'HOLE #f '())) ; inital partial program
  (define omega0 '())  ; initial knowledge base
  (define ds0 '())     ; initial decision history
  (define pps0 (list (cons 0 P0))) ; initial partial program history
  (define l0 0)        ; initial decision level
  (define (print-res r) (if (Partial? r) (print-partial r) (print r)))
  ;; wtd takes partial prog, knowledge base, decision history, 
  ;; partial program history
  ;;
  (define (wtd P Omega l ds pps)
    (print-partial P)
    (print (list Omega l ds pps))
    ; decide to fill hole H with production pr
    (match-define (cons H pr) (Decide P gamma Phi Omega gamma)) ; TODO one gama
    (print 'decide)
    (print H)
    (print 'decide)
    ; propagate assignment and update partial program
    (define P1 (Propagate P gamma H pr Omega gamma)) ; TODO should also update decision history
    ; update decision history and partial program history
    (define ds1 (cons (cons H l) ds)) ; TODO should update with propagate restul NOTE H should be int
    (define pps1 (cons (cons (+ l 1) P1) pps))
    ; check for conflict
    (define kappa (CheckConflict P1 Psi Phi))
    ; backtrack if there is conflict
    (match-let* (; analyze conflict and return MUC
                 [OmegaK (AnalyzeConflict P1 gamma Psi kappa)]
                 ; update knowledge base
                 [Omega1 (if (not (null? kappa))
                             (append Omega OmegaK)
                             Omega)]
                 ; update partial program and decision history
                 [(list P2 pps2 ds2 l2) (if (not (null? kappa))
                                         (backtrack OmegaK ds1 pps1)
                                         (list P1 pps1 ds1 (+ l 1)))])
      (print 'OmegaK)
      (print OmegaK)
      (print Omega1)
      (print-partial P2)
      (cond
        [(Unsat P2 Omega1 gamma) #f]
        [(null? (Holes P2)) P2]
        [else (wtd P2 Omega1 l2 ds2 pps2)])))    
  (print-res (wtd P0 omega0 l0 ds0 pps0)))

(synth R1 Psi0 '(and (= x2 (insert 1 (insert 2 nil))) (= x1 (insert 6 (insert 8 nil))) (= (insert 6 nil) y)))