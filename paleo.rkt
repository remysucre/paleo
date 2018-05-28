#lang racket
(require rackunit "conflict.rkt" "decide.rkt" "partial.rkt")

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

; From Problem Analysis to Data Definitions
;
; Identify the information that must be represented and how it is represented in
; the chosen programming language. Formulate data definitions and illustrate
; them with examples.

;https://docs.racket-lang.org/htdp-langs/advanced.html?q=grammar#%28form._%28%28lib._lang%2Fhtdp-advanced..rkt%29._define-datatype%29%29

; G: Standard grammar rules
(struct Grammar (V sigma R S))
    ; V: Non-terminals
(define V '(N L T)) ; Set of rule names (e.g. {N T P A C CL F} in paper)
    ; Sigma: terminals
(define sigma (append `,(range 0 11)
                      '(x1 x2 x3 x4 x5 last head sum maximum minimum)
                      '(take filter sort reverse)
                      '(geqz leqz eqz))) ; Set of terminals (e.g. {x -3 -2 -1 0 1 2 3 last head access ...})
    ; R: production rules
(define R (make-immutable-hash
           (list (cons 'N (append `,(range 0 11) '(x1 x2 x3 x4 x5) '((last L) (head L) (sum L) (maximum L) (minimum L))))
             (cons 'L (list '(take L N) '(filter L T) '(sort L) '(reverse L)))
             (cons 'T '(geqz leqz eqz))))) ; List of tuples of names and arguments (e.g. {(L, sort(L) reverse(L) filter(L,T,N) ...) ...})

; S: start symbol
(define S 'N) ; Specific rule: non-terminal name

; last(L) | head(L) | sum(L) | maximum(L) | minimum(L) | take(L,N) | filter(L,T)| sort(L) | reverse(L) |  geqz | leqz | eqz
; Psi: semantics - mapping from terminals to first-order formulae

 ; terminals -> formulae
#;(define Psi #hash((last    . "Lin.len ≥ 1 ∧ Lout .len = 1 ∧ Lin.max ≥ Lout.max ∧ Lin.min ≤ Lout.min ∧ Lout.first = Lin.last ∧ Lout.last = Lin.last")
                  (head    . "Lin.len ≥ 1 ∧ Lout .len = 1 ∧ Lin.max ≥ Lout.max ∧ Lin.min ≤ Lout.min ∧ Lout.first = Lin.first ∧ Lout.last = Lin.first")
                  (sum     . "Lin.len ≥ 1 ∧ Lout .len = 1")
                  (maximum . "Lin.len > 1 ∧ Lout.len = 1 ∧ Lin.max = Lout.max ∧ Lout.min ≥ Lin.min")
                  (minimum . "Lin.len > 1 ∧ Lout.len = 1 ∧ Lin.max ≥ Lout.max ∧ Lout.min = Lin.min")
                  (take    . "Lout.len < Lin.len Lin.max ≥ Lout.max Lin.min ≤ Lout.min k > 0 ∧ Lin.len > k Lin.first = Lout.first")
                  (filter  . "Lout.len < Lout.len Lout.max ≤ Lin.max Lout.min ≥ Lin.min")
                  (sort    . "Lout.len = Lin.len > 1 ∧ Lin.max = Lout.max ∧ Lin.min = Lout .min")
                  (reverse . "Lout.len = Lin.len > 1 ∧ Lin.max = Lout.max ∧ Lin.min = Lout.min ∧ Lin.first = Lout.last ∧ Lin.last = Lout.first")))

; Phi: specification - user defined smt formulae
(define Phi '(and (>= (len x1) 1)
                  (= 1 (len y))
                  (>= (max x1) (max y))
                  (<= (min x1) (min y))
                  (> (head x2) 0 )
                  (= (len x2) 1)
                  (> (len x1) (head x2)))) ; Look at bottom of paper

; P: partial program - AST with holes, built from S via production rules
(define P1 '(0 N head
                (1 L take
                    ((3 L filter ((7 L x1)
                                  (8 T HOLE)))
                     (4 N HOLE)))))

; Omega: knowledge base - set of SAT formulae
(define Omega '((disj (neg c 0 filter) (neg c 2 eqz))
                (disj (neg c 0 filter) (neg c 2 leqz))))

; H: hole - unfilled piece of the partial program
(define hole 4)

; p: candidate production - to fill in the hole
(define p1 'filter)

; kappa: conflict
; (y ≤ x1.max, N0, head), (y.max ≤ x1.max, N1, take), (y.max ≤ x1.max,N3,filter), (y = x1,N7,x1)
(define kappa '(((leq y (max x1)) 0 'head)
                ((leq ((max y) (max x1))) 1 'take)
                ((leq (max y) (max x1)) 3 'filter)
                ((= y x1) 7 'x1)))

; Signature, Purpose Statement, Header
;
; State what kind of data the desired function consumes and produces. Formulate
; a concise answer to the question what the function computes. Define a stub
; that lives up to the signature.

(define (Root S)
  (list 0 S 'HOLE))

(check-equal? (Root S) '(0 N HOLE))

(define (IsConcrete P)
  (not (member 'HOLE (flatten P))))

(define (synth gamma Psi Phi)
  ;; initialize variables
  ;;
  (define P0 (Partial 5 'N 'HOLE #f '())) ; inital partial program
  (define omega0 '())  ; initial knowledge base
  (define ds0 '())     ; initial decision history
  (define pps0 (list P0)) ; initial partial program history
  (define l0 1)        ; NOTE might be off by 1

  ;; wtd takes partial prog, knowledge base, decision history, 
  ;; partial program history
  ;;
  (define (wtd P Omega l ds pps)
    ; decide to fill hole H with production pr
    (match-define (cons H pr) (Decide P gamma Phi Omega gamma)) ; TODO one gama
    ; propagate assignment and update partial program
    (define P1 (Propagate P gamma H pr Omega gamma)) ; TODO should also update decision history
    ; update decision history and partial program history
    (define ds1 (cons (cons l H) ds)) ; TODO should update with propagate restul NOTE H should be int
    (define pps1 (cons (cons l P1) pps))
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
                 [(list P2 ds2 pps2) (if (not (null? kappa))
                                         (backtrack OmegaK ds pps)
                                         (list P1 ds1 pps1))])
      (cond
        [(Unsat Omega1) #f]
        [(IsConcrete P2) P2]
        [else (wtd P2 Omega1 (+ 1 l) ds2 pps2)])))
  (wtd P0 omega0 l0 ds0 pps0))
(synth R1 Psi0 Phi)