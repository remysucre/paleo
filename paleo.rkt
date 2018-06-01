#lang racket
(require rackunit "conflict.rkt" "decide.rkt" "partial.rkt")
(provide (all-defined-out))

; BACKTRACK
; 
; backtrack finds the second highest decision level in the MUC omega according to ds
; then returns the partial program at that level from pps
; as well as reverting ds and pps to that level

; takes in a conflict, the decision history and the partial progam history
; returns the partial program at the second highest decision level in the conflict
(define (backtrack omega ds pps)
  ;(print omega)
  ;(print ds)
  ;(print pps)
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
(define (synth gamma Psi in out *debug* *learn?*)
  (define P0 (Partial 1 'N 'HOLE #f '()))
  ;(define P0 (Partial 1 'N 'head #t (list (Partial 2 'L 'HOLE #f '())))) ; inital partial program
  (define omega0 '())  ; initial knowledge base
  (define ds0 '())     ; initial decision history
  (define pps0 (list (cons 0 P0))) ; initial partial program history
  (define l0 0)        ; initial decision level
  (define (print-res r) (if (Partial? r) (print-partial r) (begin (print r) (newline))))
  ;; wtd takes partial prog, knowledge base, decision history, 
  ;; partial program history
  ;;
  (define Phi `(and (= x1 ,(List->Z3 in)) (= y ,(List->Z3 out))))
  (define (wtd P Omega l ds pps)
    ;(print-partial P)
    ;(print (list Omega l ds pps))
    ; decide to fill hole H with production pr
    ;(print-partial P)
    (match-define (cons H pr) (Decide P gamma in out Omega gamma)) ; TODO one gama
    ;(print 'decide)
    ;(print H)
    ;(print 'decide)
    ; propagate assignment and update partial program
    (define P1 (Propagate P gamma H pr Omega gamma)) ; TODO should also update decision history
    (when *debug* (print-partial P1))
    ; update decision history and partial program history
    (define ds1 (cons (cons H l) ds)) ; TODO should update with propagate restul NOTE H should be int
    (define pps1 (cons (cons (+ l 1) P1) pps))
    ; check for conflict
    (define kappa (CheckConflict P1 Psi Phi in out))
    ; backtrack if there is conflict
    (match-let* (; analyze conflict and return MUC
                 [OmegaK (AnalyzeConflict P1 gamma Psi kappa in *learn?*)]
                 ; update knowledge base
                 [Omega1 (if (not (null? kappa))
                             (append OmegaK Omega)
                             Omega)]
                 ; update partial program and decision history
                 [(list P2 pps2 ds2 l2) (if (not (null? kappa))
                                         (backtrack OmegaK ds1 pps1)
                                         (list P1 pps1 ds1 (+ l 1)))])

      (when *debug* (eprintf "Omega: ~s\n\n" Omega1))
      ;(print 'OmegaK)
      ;(print OmegaK)
      ;(print Omega1)
      ;(print-partial P2)
      (cond
        [(Unsat P2 Omega1 gamma) #f]
        [(null? (Holes P2)) P2]
        [else (wtd P2 Omega1 l2 ds2 pps2)])))
  (define start (current-inexact-milliseconds))
  (define res (wtd P0 omega0 l0 ds0 pps0))
  (define end (current-inexact-milliseconds))
  (printf "\nInput: ~s\nOutput: ~s\nTime: ~ss\nLearning was ~a\nResult:\n"
          in
          out
          (* 0.001 (round (- end start)))
          (if *learn?* "on" "off"))
  (print-res res))
