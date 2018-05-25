#lang racket

(require rackunit "solver.rkt")

(provide (all-defined-out))

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

(define (SMTSolve f) (solve f))

(define (subst x y zs)
  (define (f z) (if (list? z) (subst x y z) (if (equal? z x) y z)))
  (map f zs))

(define (substs xs ys z)
  (let ((xys (map cons xs ys)))
    (foldr (lambda (xy z) (subst (car xy) (cdr xy) z)) z xys)))

(substs '(x y z) '(1 2 3) '(z (y x z) (y x (z))))

(define (Node f) (if (member 'y f) 0 (get-vn f)))

(define (get-i v) (string->number (substring (symbol->string v) 1)))

(define (get-vn f)
  (let ((vs (filter (lambda (x) (string-prefix? (symbol->string x) "v")) f)))
    (argmax (lambda (x) x) (map get-i vs))))

(get-vn '(v3 x v5))

(define (Rename p)
  (define (rn x) '()) ; TODO
  (let ((vmax (argmax get-i (filter (lambda (x) (string-prefix? (symbol->string x) "v")) p))))
    (map rn p)))