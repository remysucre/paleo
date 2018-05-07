;(declare-fun map ((List Int)) (List Int))
(define-fun-rec len ((l (List Int))) Int
    (ite (= nil l)
        0
        (+ 1 (len (tail l)))))

(declare-fun _map ((List Int)) (List Int))
(assert 
    (forall ((lin (List Int)) (lout (List Int)))
        (= (len lin) (len (_map lin)))))
        
(assert (and (= 1 (len (_map nil)))))
        
(check-sat)