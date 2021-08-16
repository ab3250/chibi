(declare (extended-bindings) (not constant-fold) (not safe))

(define x1 keyword1:)
(define x2 keyword2:)

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
  (println (which (and x y)))
  (println (if (and x y) 11 22))
  (println (which (and (##not x) y)))
  (println (if (and (##not x) y) 11 22))
  (println (which (and x (##not y))))
  (println (if (and x (##not y)) 11 22))
  (println (which (and (##not x) (##not y))))
  (println (if (and (##not x) (##not y)) 11 22))
  (println (which (##not (and x y))))
  (println (if (##not (and x y)) 11 22))
  (println (which (##not (and (##not x) y))))
  (println (if (##not (and (##not x) y)) 11 22))
  (println (which (##not (and x (##not y)))))
  (println (if (##not (and x (##not y))) 11 22))
  (println (which (##not (and (##not x) (##not y)))))
  (println (if (##not (and (##not x) (##not y))) 11 22)))

(define (test x)
  (test2 x x1)
  (test2 x x2))

(test x1)
(test x2)
