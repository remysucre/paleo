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
(define (CheckConflict P Psi Phi in out)
  ;(print 'checkcstart)
  ;(print-partial P)
  (define (phi-n-xn muc)
    (let* ([id (map string->number (string-split (substring (symbol->string muc) 1) "p"))]
           [xn (Partial-Terminal (Lookup-By-ID P (car id)))]
           [ph (if (not (null? (Partial-Children (Lookup-By-ID P (car id)))))
                   (list-ref (sem xn Psi) (- (second id) 1))
                   (sem xn Psi))])
      (list ph (car id) xn)))
  (define (declare-xs phi)
    (cons '(declare-const y (List Int))
          (map (lambda (x) (list 'declare-const x '(List Int)))
               (filter (lambda (c) (string-prefix? (symbol->string c) "x"))
                       (set->list (list->set (filter symbol? (flatten phi))))))))
  (let* ([Phi-P (InferSpec P Psi)]
         [concrete (if (null? (Holes P))
                       (for/list ([clause (Assert P in)])
                         `(assert ,clause))
                   `((assert true)))]
         [encoding (append (declare-xs Phi) Phi-P (list (list 'assert (list '! Phi ':named 'aphi))))]
         [concrete-invalid?
          (if (and (null? (Holes P)))
              (if (equal? (Execute-Node P in) out)
                  #f
                  (let* ([id (Partial-Size P)]
                         [node (Lookup-By-ID P id)]
                         [xn (Partial-Terminal node)]
                         [ph (if (not (null? (Partial-Children node)))
                                 (list-ref (sem xn Psi) (- id 1))
                                 (sem xn Psi))])
                    (list (list ph id xn))))
              #f)]
         [psi0 (if concrete-invalid? concrete-invalid? (SMTSolve encoding))]
         [psi (if (list? psi0) psi0 '())]
         [k (if concrete-invalid? concrete-invalid? (map phi-n-xn (filter (lambda (x) (not (equal? x 'aphi))) psi)))])
    #;(print k) k))

(define (InferSpec p Psi)
  (define (declare-vs p)
    ;(print-partial p)
    (for/list ([n p])
      ;(eprintf "~s ~s\n" (Partial-ID n) (v_s (Partial-ID n)))
        (list 'declare-const (v_s (Partial-ID n)) '(List Int))))
  (define (phi-p p Psi)
    (subst (v_s (Partial-ID p)) 'y (phi-n p Psi)))
  (define (phi-n p Psi)
    (for/list ([n p] #:when (Partial-Filled? n))
      (define rules (psi-n n Psi))
      (define prod 0)
      (for/list ([rule rules])
        (set! prod (+ prod 1))
        (list 'assert (list '! rule ':named
                            (string->symbol (string-append "a" (number->string (Partial-ID n)) "p" (number->string prod))))))))
  (define (psi-n p Psi)
    (if (not (Partial-Filled? p))
        'true
        (if (null? (Partial-Children p))
            (if (number? (Partial-Terminal p))
                (list (list '= (v_s (Partial-ID p)) (list 'insert (Partial-Terminal p) 'nil)))
                (list (list '= (v_s (Partial-ID p)) (Partial-Terminal p))))
            (substs (xs-of (sem (Partial-Terminal p) Psi))
                    (C (Partial-Children p))
                    (subst 'y (v_s (Partial-ID p)) (sem (Partial-Terminal p) Psi))))))
  (define (xs-of s)
    (remove-duplicates
     (filter (lambda (x) (and (symbol? x) (string-prefix? (symbol->string x) "x"))) (flatten s)))) ; do this properly
  (define (C cs) (map (lambda (n) (v_s (Partial-ID n))) cs))
  (define (v_s id) (string->symbol (string-append "v" (number->string id))))
  (let ([vs (declare-vs p)]
        [phi-ns (apply append (phi-p p Psi))]) (append vs phi-ns)))

(define (sem p Psi)
  #;(print p)
  #;(print (number? p))
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
(define (AnalyzeConflict P gamma Psi kappa in *learn?*)
  ;(eprintf "Kappa: ~s\n\n" kappa)
  (define (xs n)
    (cons '(declare-const x1 (List Int))
          (for/list ([x (in-range 1 n)])
            `(declare-const ,(string->symbol (format "x~s" (+ x 1))) (List Int)))))
  (list
   (for/list ([clause kappa])
     (match-define (list phi node p0) clause)
     (define rules gamma)
     (define A (Lookup-By-ID P node))
     (define sigma
       (if *learn?*
           (for*/list ([prod (dict-ref rules (Partial-Non-Terminal A))]
                       #:when (list? (SMTSolve `(,@(xs (max (length (Production-Children prod)) (length (Partial-Children A))))
                                                 (declare-const y (List Int))
                                                 (assert (= x1 ,(List->Z3 in)))
                                                 (assert
                                                  (and ,@(if (list? prod)
                                                             (sem (Production-Terminal prod) Psi)
                                                             `(,(sem (Production-Terminal prod) Psi)))
                                                       (not ,phi)))))))
             (cons (Partial-ID A) (Production-Terminal prod)))
           (list (cons node p0))))
     ;(eprintf "Sigma: ~s, nots: ~s\n" sigma (list (cons node p0)))
     sigma)))
  
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
   (list (cons 'N (append `,(range 0 5) '(x1) '((last L) (head L) (sum L) (maximum L) (minimum L))))
         (cons 'L (append '(x1) (list '(take L N) '(sort L) '(reverse L)))))))

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

(define Psi (make-immutable-hash
             `((minimum . (,(geqlen 1 'x1) ,(eqlen 1 'y) (>= (max x1) (max y)) (= (min x1) (min y))))
               (maximum . (,(geqlen 1 'x1) ,(eqlen 1 'y) (= (max x1) (max y)) (<= (min x1) (min y))))
               (last . (,(geqlen 1 'x1) ,(eqlen 1 'y) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (last x1)) (= (last y) (last x1))))
               (head . (,(geqlen 1 'x1) ,(eqlen 1 'y) (>= (max x1) (max y)) (<= (min x1) (min y)) (= (first y) (first x1)) (= (last y) (first x1))))
               (sum . (,(geqlen 1 'x1) ,(eqlen 1 'y) (> (head y) (max x1))))
               (take . ((< (len y) (len x1)) ,(eqlen 1 'x2) (= (len y) (head x2)) (>= (max x1) (max y)) (<= (min x1) (min y)) (> (head x2) 0) (> (len x1) (head x2)) (= (first x1) (first y))))
               (reverse . ((= (len y) (len x1)) ,(geqlen 2 'x1) (= (max x1) (max y)) (= (min x1) (min y)) (= (first x1) (last y)) (= (last x1) (first y))))
               (sort . ((= (len y) (len x1)) ,(geqlen 2 'x1) (= (max x1) (max y)) (= (min x1) (min y)))))))

(define (List->Z3 xs)
  (define l (if (number? xs) (list xs) xs))
  (for/fold ([nil 'nil]) ([x (reverse l)])
    `(insert ,x ,nil)))

(define (Maximum xs)
  (list (argmax identity xs)))

(define (Minimum xs)
  (list (argmax (lambda (x) (- x)) xs)))

(define (Last xs)
  (list (last xs)))

(define (Head xs)
  (list (car xs)))

(define (Reverse xs)
  (reverse xs))

(define (Sort xs)
  (sort xs <))

(define (Sum xs)
  (list (foldl + 0 xs)))

(define (Take xs n)
  (if (or (> (car n) (length xs)) (= 0 (car n)))
      xs
      (take xs (car n))))

(define (get-fun prod)
  (match prod
    ['take Take]
    ['sort Sort]
    ['last Last]
    ['head Head]
    ['maximum Maximum]
    ['minimum Minimum]
    ['sum Sum]
    ['reverse Reverse]
    [(? number?) (lambda () (list prod))]))

(define (Execute-Node P in)
  (if (or (not (null? (Holes P))) (eq? 'x1 (Partial-Terminal P)))
      in
      (apply (get-fun (Partial-Terminal P))
             (for/list ([child (Partial-Children P)]) (Execute-Node child in)))))

(define (Assert P in)
  (define (v_s id) (string->symbol (string-append "v" (number->string id))))
  (if (null? (Holes P))
      (for/list ([node P])
        `(= , (v_s (Partial-ID node)) ,(List->Z3 (Execute-Node node in))))
      '()))

;(sem 0 Psi)