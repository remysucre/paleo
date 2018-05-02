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
