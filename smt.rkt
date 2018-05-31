#lang racket

(require rackunit "solver.rkt")

(provide (all-defined-out))

(define produce-unsat '(set-option :produce-unsat-cores true))

(define max3
  '(define-fun-rec max ((xs (List Int))) Int
     (let ((h (head xs)) (t (tail xs)))
       (ite (= t nil) h
            (let ((mt (max t)))
              (ite (> h mt) h mt))))))

(define min3
  '(define-fun-rec min ((xs (List Int))) Int
     (let ((h (head xs)) (t (tail xs)))
       (ite (= t nil) h
            (let ((mt (min t)))
              (ite (< h mt) h mt))))))

(define len3
  '(define-fun-rec len ((xs (List Int))) Int
     (ite (= xs nil) 0 (+ 1 (len (tail xs))))))

(define first3
  '(define-fun-rec first ((xs (List Int))) Int (head xs)))

(define last3
  '(define-fun-rec last ((xs (List Int))) Int 
     (ite (= nil (tail xs)) (head xs) (last (tail xs)))))

(define (conj x y) (list 'and x y))

(define (eqlen len list)
  `(and
    ,@(for/list ([x (in-range 0 len)])
        `(not (=
               nil
               ,(for/fold ([default list]) ([y (in-range 0 x)])
                          `(tail ,default)))))
    (=
     ,(for/fold ([default list]) ([y (in-range 0 len)])
                `(tail ,default))
    nil)))

(define (leqlen len list)
  `(or
    ,@(for/list ([x (in-range 0 (+ len 1))])
        `(=
          nil
          ,(for/fold ([default list]) ([y (in-range 0 x)])
             `(tail ,default))))))

(define (geqlen len list)
  `(and
    ,@(for/list ([x (in-range 0 len)])
        `(not (=
               nil
               ,(for/fold ([default list]) ([y (in-range 0 x)])
                          `(tail ,default)))))))
    
(define (SMTSolve f)
  (let ([ans (solve (append (list produce-unsat len3 min3 max3 first3 last3) f))]) #;(print f) #;(print ans) ans))

(define (SATSolve f)
  (let ([ans (solve (append (list produce-unsat) f))]) ans))

(define (subst x y zs)
  (define (f z) (if (list? z) (subst x y z) (if (equal? z x) y z)))
  (map f zs))

(define (substs xs ys z)
  (let ((xys (map cons xs ys)))
    (foldr (lambda (xy z) (subst (car xy) (cdr xy) z)) z xys)))

;(substs '(x y z) '(1 2 3) '(z (y x z) (y x (z))))

(define (Node f) (if (member 'y f) 0 (get-vn f)))

(define (get-i v) (string->number (substring (symbol->string v) 1)))

(define (get-vn f)
  (let ((vs (filter (lambda (x) (string-prefix? (symbol->string x) "v")) f)))
    (argmax (lambda (x) x) (map get-i vs))))

;(get-vn '(v3 x v5))


(define (make-x i) (string->symbol (string-append "x" (number->string i))))

(define (deep-map f xs) (map (lambda (x) (if (list? x) (deep-map f x) (f x))) xs))

;(deep-map (lambda (x) (+ x 1)) (list 1 1 (list 1 (list 1 1) 1) 1 1 (list (list 1 1) 1)))

(define (Rename p)
  (let ((vs (sort (filter (lambda (x) (string-prefix? (symbol->string x) "v")) (flatten p)) #:key get-i <)))
    (define (rn x) (if (member x vs) (if (= (index-of vs x) 0) 'y (make-x (index-of vs x))) x))
    (deep-map rn p)))
;(Rename '(leq (max v1) (max v3)))