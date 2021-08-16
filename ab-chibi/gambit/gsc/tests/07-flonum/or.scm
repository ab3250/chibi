(declare (extended-bindings) (not constant-fold) (not safe))

(define a 3.125)
(define b -1.25)

(define (test2 x y)
  (println (or x y))
  (println (if (or x y) 11 22))
  (println (or (##not x) y))
  (println (if (or (##not x) y) 11 22))
  (println (or x (##not y)))
  (println (if (or x (##not y)) 11 22))
  (println (or (##not x) (##not y)))
  (println (if (or (##not x) (##not y)) 11 22))
  (println (##not (or x y)))
  (println (if (##not (or x y)) 11 22))
  (println (##not (or (##not x) y)))
  (println (if (##not (or (##not x) y)) 11 22))
  (println (##not (or x (##not y))))
  (println (if (##not (or x (##not y))) 11 22))
  (println (##not (or (##not x) (##not y))))
  (println (if (##not (or (##not x) (##not y))) 11 22)))

(define (test x)
  (test2 x a)
  (test2 x b))

(test a)
(test b)
