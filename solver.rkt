#lang racket

(require racket/runtime-path racket/path)

(provide 
 cnf/c           ; contract? that recognizes CNF specifications
 interpretation? ; contract? that recognizes CNF interpretations
 solver?         ; contract? that recognizes solver (descriptions)
 solve           ; (->* (cnf/c) [solver?] (or/c #f interpretation?))
 glucose         ; (->* () [path-string?] solver?)
 lingeling       ; (->* () [path-string?] solver?)
)

; We represent a CNF formula as a list of clauses, where each clause is itself 
; a list of non-zero integers.  Positive integers represent positive literals 
; (i.e., positive occurrences of the ith variable), and negative integers 
; represent negative literals.
(define cnf/c (listof (listof integer?)))

; An interpretation is represented as a list of literals, sorted in the 
; increasing order of variable identifiers.  
(define interpretation? (listof integer?))

; A solver description contains the path to a solver executable
; and any options to be passed to the solver.
(struct solver (path options) #:transparent)

; We assume that the solvers are in the bin/ folder.
(define-runtime-path bin (build-path "bin"))

; Returns a solver description for the Glucose SAT solver.
(define (glucose [path (build-path bin "glucose")])
  (solver path '("-model" "-verb=0")))

; Returns a solver description for the Lingeling SAT solver.
(define (lingeling [path (build-path bin "lingeling")])
  (solver path '("-q")))

; Invokes a SAT solver on the given CNF and returns either an interpretation?, 
; if the formula is satisfiable, or #f otherwise.  The lingeling solver is the 
; default value for the solver arugment.
(define (solve cnf [solver (lingeling)])
  (unless (file-exists? (solver-path solver))
    (error 'solve "error: the solver `~a` does not exist. did you install it?" (solver-path solver)))
  (define-values (process out in err)
    (apply subprocess #f #f #f (solver-path solver) (solver-options solver)))
  (with-handlers ([exn:break? (lambda (e) 
                                (subprocess-kill process #t)
                                (raise e))])
    (write-cnf cnf in)
    (define sol (read-solution out))
    (subprocess-kill process #t)
    (close-input-port out)
    (close-input-port err)
    (if (list? sol) (sort sol < #:key abs) sol)))
  
(define (write-cnf cnf [port (current-output-port)])
  (define vars (abs (argmax abs (map (curry argmax abs) cnf))))
  (define clauses (length cnf))
  (fprintf port "p cnf ~a ~a\n" vars clauses)
  (for ([clause cnf])
    (for ([lit clause])
      (fprintf port "~a " lit))
    (fprintf port "0\n"))
  (flush-output port)
  (close-output-port port))
    
(define (read-solution port)
  (let outer ([line (read-line port 'any)])
    (pretty-print line)
    (match line
      [(? eof-object?)
       (error 'read-solution "expected a line starting with s and ending with SATISFIABLE or UNSATISFIABLE")]
      [(regexp #px"^\\s*s\\s+UNSATISFIABLE\\s*$") #f]
      [(regexp #px"^\\s*s\\s+SATISFIABLE\\s*$")
       (let inner ([line (read-line port 'any)])
         (match line
           [(? eof-object?) (error 'read-solution "expected a line starting with v and ending with 0")]
           [(regexp #px"^\\s*v\\s+(.+)$" (list _ (app string->literals lits)))
            (if (= 0 (last lits))
                (drop-right lits 1)
                (append lits (inner (read-line port 'any))))]
           [_ (inner (read-line port 'any))]))]
      [_ (outer (read-line port 'any))])))

(define (string->literals str)
  (map string->number (string-split str #:trim? #t)))
