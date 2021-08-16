(declare (extended-bindings) (not constant-fold) (not safe))

(define x1 '#u64())
(define x2 '#u64(1 2 18446744073709551615))

(define (which z)
  (cond ((##eq? z x1)
         "=x1")
        ((##eq? z x2)
         "=x2")
        ((##eq? z #f)
         "=#f")
        (else
         "not =x1 or =x2 or =#f")))

(define (test2 x y)
  (println (which (or x y)))
  (println (if (or x y) 11 22))
  (println (which (or (##not x) y)))
  (println (if (or (##not x) y) 11 22))
  (println (which (or x (##not y))))
  (println (if (or x (##not y)) 11 22))
  (println (which (or (##not x) (##not y))))
  (println (if (or (##not x) (##not y)) 11 22))
  (println (which (##not (or x y))))
  (println (if (##not (or x y)) 11 22))
  (println (which (##not (or (##not x) y))))
  (println (if (##not (or (##not x) y)) 11 22))
  (println (which (##not (or x (##not y)))))
  (println (if (##not (or x (##not y))) 11 22))
  (println (which (##not (or (##not x) (##not y)))))
  (println (if (##not (or (##not x) (##not y))) 11 22)))

(define (test x)
  (test2 x x1)
  (test2 x x2))

(test x1)
(test x2)
