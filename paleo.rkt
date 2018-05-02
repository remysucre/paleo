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

; G grammar, Ψ semantics, Φ spec
; Ω knowledge base Ω (set of learned lemmas)
; P partial program, S start symbol
; (H,p) hole and candidate production
; κ conflict

; Signature, Purpose Statement, Header
;
; State what kind of data the desired function consumes and produces. Formulate
; a concise answer to the question what the function computes. Define a stub
; that lives up to the signature.

; G: Standard grammar rules
    ; V: Non-terminals
    ; Sigma: terminals
    ; R: production rules
    ; S: start symbol
; Psi: semantics - mapping from terminals to first-order formulae
; Phi: specification - user defined smt formulae
; Omega: knowledge base - set of SAT formulae
; P: partial program - AST with holes, built from S via production rules
; H: hole - unfilled piece of the partial program
; p: candidate production - to fill in the hole
; kappa: conflict

;https://docs.racket-lang.org/htdp-langs/advanced.html?q=grammar#%28form._%28%28lib._lang%2Fhtdp-advanced..rkt%29._define-datatype%29%29

(define V '(N T P A C CL F)) ; Set of rule names (e.g. {N T P A C CL F} in paper)
(define sigma '(x -3 -2 -1 0 1 2 3 last head access ...)) ; Set of terminals (e.g. {x -3 -2 -1 0 1 2 3 last head access ...})
(define R '((L, sort(L) reverse(L) filter(L T N) ...) ...)) ; List of tuples of names and arguments (e.g. {(L, sort(L) reverse(L) filter(L,T,N) ...) ...})
(define S 'N) ; Specific rule: non-terminal name
(struct Grammar (V sigma R S))
(define Psi #hash((inc . (lambda (x y) (= y (+ x 1)))))) ; terminals -> formulae
(define Phi (lambda (Lin Lout) (and (>= (len Lin) 1) (= (len Lout) 1)))) ; Look at bottom of paper

(define Omega '((or (not c0...) (not c1...) ...) ...))

(define (paleo G Psi Phi))

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
