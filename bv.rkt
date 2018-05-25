#lang racket

(require (only-in racket [= racket/=]) racket/syntax)
(provide define-fragment define set! if =
         bvneg bvnot bvule bvult bvuge bvugt bvsle bvslt bvsge bvsgt  
         bvsdiv  bvsrem  bvshl  bvlshr  bvashr  bvsub
         bvor bvand bvxor bvadd bvmul
         fragment? fragment-ast finitize)

; This module defines the syntax and semantics of the BV language.
; For the sake of brevity, the implementation does not enforce the 
; well-formedness constraints specified in HW2.  We'll just make sure 
; not to construct ill-formed programs.

; A fragment object consists of an abstract syntax tree (AST) for a 
; BV program, represented as a nested list of symbols (an s-expression), 
; as well as a procedure corresponding to that AST.  Applying a fragment 
; to inputs has the same effect as applying its procedure to those inputs.
(struct fragment (ast proc)
  #:transparent
  #:property prop:procedure
  [struct-field-index proc])

(define-syntax (define-fragment stx)
  (syntax-case stx (define-fragment return)
    [(define-fragment (id in ...)
       stmt ...
       (return expr))
     (quasisyntax/loc stx 
       (define id
         (fragment 
          '#,stx
          (procedure-rename
           (lambda (in ...)
            stmt ...
            expr)
           'id))))]))

(define-syntax-rule
  (unary-ops [id semantics] ...)
  (define-values (id ...) 
    (values (lambda (x) (finitize (semantics (finitize x)))) ...)))

(define-syntax-rule
  (binary-ops [id semantics] ...)
  (define-values (id ...) 
    (values (lambda (x y) (finitize (semantics (finitize x) (finitize y)))) ...)))

(define-syntax-rule
  (binary-predicates [id semantics] ...)
  (define-values (id ...) 
    (values (lambda (x y) (semantics (finitize x) (finitize y))) ...)))

(define-syntax-rule
  (unsigned-binary-predicates [id semantics] ...)
  (define-values (id ...) 
    (values (lambda (x y) 
              (let ([fx (finitize x)]
                    [fy (finitize y)])
                (if (equal? (< x 0) (< y 0)) 
                    (semantics fx fy) 
                    (semantics fy fx)))) ...)))

(define-syntax-rule
  (nary-ops [id semantics] ...)
  (define-values (id ...)
    (values (lambda (x . xs)
              (finitize (apply semantics (finitize x) (map finitize xs)))) ...)))

(unary-ops [bvneg -] [bvnot bitwise-not])

(binary-ops [bvsdiv quotient] [bvsrem remainder] [bvsub -])

(define (bvshl x y)
  (let ([fx (finitize x)]
        [fy (finitize y)])
    (if (or (< fy 0) (> fy 31)) 0 (arithmetic-shift fx fy))))

(define (bvlshr x y)
  (let ([fx (finitize x)]
        [fy (finitize y)])
    (if (or (< fy 0) (> fy 31)) 0 (arithmetic-shift fx (- fy)))))

(define (bvashr x y)
  (let ([fx (bitwise-and x (bitwise-not (arithmetic-shift -1 32)))]
        [fy (finitize y)]) 
    (if (or (< fy 0) (> fy 31)) (sgn x) (arithmetic-shift fx (- fy)))))

(nary-ops [bvadd +] [bvmul *] [bvand bitwise-and] [bvor bitwise-ior] [bvxor bitwise-xor])

(define (= x y)
  (cond [(and (integer? x) (integer? y)) (racket/= (finitize x) (finitize y))]
        [(and (boolean? x) (boolean? y)) (equal? x y)]
        [else (error '= "cannot compare values of different types:  (= ~a ~a)" x y)]))

(binary-predicates [bvsle <=] [bvslt <] [bvsge >=] [bvsgt >])
(unsigned-binary-predicates [bvule <=] [bvult <] [bvuge >=] [bvugt >])

; Racket's numeric operations work on infinite precision 
; values.  For BV, we need to restrict values to 32-bit precision.
(define (finitize v)
  (unless (integer? v)
    (error 'BV "expected an integer, given ~a" v))
  (let* ([mask (arithmetic-shift -1 32)]
         [masked (bitwise-and (bitwise-not mask) v)])
    (if (bitwise-bit-set? masked 31)
        (bitwise-ior mask masked)  
        masked)))

