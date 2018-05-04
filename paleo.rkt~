#lang racket

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
(define V '(N T P A C CL F)) ; Set of rule names (e.g. {N T P A C CL F} in paper)
    ; Sigma: terminals
(define sigma '(x -3 -2 -1 0 1 2 3 last head access ...)) ; Set of terminals (e.g. {x -3 -2 -1 0 1 2 3 last head access ...})
    ; R: production rules
(define R '((L, sort(L) reverse(L) filter(L T N) ...) ...)) ; List of tuples of names and arguments (e.g. {(L, sort(L) reverse(L) filter(L,T,N) ...) ...})
    ; S: start symbol
(define S 'N) ; Specific rule: non-terminal name

; Psi: semantics - mapping from terminals to first-order formulae
(define Psi #hash((inc . (lambda (x y) (= y (+ x 1)))))) ; terminals -> formulae

; Phi: specification - user defined smt formulae
(define Phi (lambda (Lin Lout) (and (>= (length Lin) 1) (= (length Lout) 1)))) ; Look at bottom of paper

; P: partial program - AST with holes, built from S via production rules
(define P1 '(0 N head
                (1 L take
                    ((3 L filter ((7 L x1)
                                  (8 T)))
                     (4 N)))))

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

(define (synth G Psi Phi) '())

; Functional Examples
;
; Work through examples that illustrate the function’s purpose.


; Function Template
;
; Translate the data definitions into an outline of the function.


; Function Definition
;
; Fill in the gaps in the function template. Exploit the purpose statement and
; the examples.


; Testing
;
; Articulate the examples as tests and ensure that the function passes all.
; Doing so discovers mistakes. Tests also supplement examples in that they help
; others read and understand the definition when the need arises—and it will
; arise for any serious program.
