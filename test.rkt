#lang reader "parser.rkt"

(Terminal N [last L] [head L] [sum L] [maximum L] [minimum L] 0 1 2 3 87 x0 x1 x2 x3)
(Terminal L [take L N] [filter L T] [sort L] [reverse L] x0 x1 x2 x3)
(Terminal T geqz leqz eqz)

(Partial p1 (head (take (filter (x1) (??)) (??))))
(Partial in (x1))
(Partial out (87))

(Synthesize test [in out] p1)

(Spec (last [Lin] Lout)
      ([Lin.len >= 1] [Lout.len = 1]
       [Lin.max >= Lout.max] [Lin.min <= Lout.min]
       [Lout.first = Lin.last] [Lout.last = Lin.last]))

(Spec (head [Lin] Lout)
      ([Lin.len >= 1] [Lout.len = 1]
       [Lin.max >= Lout.max] [Lin.min <= Lout.min]
       [Lout.first = Lin.first] [Lout.last = Lin.first]))

(Spec (sum [Lin] Lout)
      ([Lin.len >= 1] [Lout.len = 1]))

(Spec (maximum [Lin] Lout)
      ([Lin.len > 1] [Lout.len = 1]
       [Lin.max = Lout.max] [Lin.min <= Lout.min]))

(Spec (minimum [Lin] Lout)
      ([Lin.len > 1] [Lout.len = 1]
       [Lin.max >= Lout.max] [Lin.min = Lout.min]))

(Spec (take [Lin k] Lout)
      ([Lout.len < Lin.len] [Lin.max >= Lout.max]
       [Lin.min <= Lout.min] [k > 0]
       [Lin.len > k] [Lin.first = Lout.first]))

(Spec (filter [Lin t] Lout)
      ([Lout.len < Lin.len] [Lin.max >= Lout.max]
       [Lin.min <= Lout.min]))

(Spec (sort [Lin] Lout)
      ([Lout.len = Lin.len] [Lin.len > 1]
       [Lin.max = Lout.max] [Lin.min = Lout.min]))

(Spec (reverse [Lin] Lout)
      ([Lout.len = Lin.len] [Lin.len > 1]
       [Lin.max = Lout.max] [Lin.min = Lout.min]
       [Lin.first = Lout.last] [Lin.last = Lout.first]))