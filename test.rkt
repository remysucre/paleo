#lang reader "parser.rkt"

(Terminal N [last L] [head L] [sum L] [maximum L] [minimum L] 0 1 2 3)
(Terminal L [take L N] [filter L T] [sort L] [reverse L] x1 x2 x3)
(Terminal T geqz leqz eqz)

(Partial (head (take (filter (x1) (??)) (??))))

(Spec (take [Lin k] Lout)
      ([Lout.len < Lin.len]
       [Lin.max ≥ Lout.max]
       [Lin.min ≤ Lout.min]
       [k > 0]
       [Lin.len > k]
       [Lin.first = Lout.first]))