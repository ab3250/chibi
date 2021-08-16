(declare (extended-bindings) (not constant-fold) (not safe))

(define zero 0.0)
(define negzero -0.0)

(define (test x y)
  (println (##fl< x y))
  (println (##fl< x 0.0))
  (println (##fl< 0.0 y))
  (println (##fl< x -0.0))
  (println (##fl< -0.0 y))
  (println (##fl< x zero))
  (println (##fl< zero y))
  (println (##fl< x negzero))
  (println (##fl< negzero y))

  (println (##fl<= x y))
  (println (##fl<= x 0.0))
  (println (##fl<= 0.0 y))
  (println (##fl<= x -0.0))
  (println (##fl<= -0.0 y))
  (println (##fl<= x zero))
  (println (##fl<= zero y))
  (println (##fl<= x negzero))
  (println (##fl<= negzero y))

  (println (##fl> x y))
  (println (##fl> x 0.0))
  (println (##fl> 0.0 y))
  (println (##fl> x -0.0))
  (println (##fl> -0.0 y))
  (println (##fl> x zero))
  (println (##fl> zero y))
  (println (##fl> x negzero))
  (println (##fl> negzero y))

  (println (##fl>= x y))
  (println (##fl>= x 0.0))
  (println (##fl>= 0.0 y))
  (println (##fl>= x -0.0))
  (println (##fl>= -0.0 y))
  (println (##fl>= x zero))
  (println (##fl>= zero y))
  (println (##fl>= x negzero))
  (println (##fl>= negzero y))

  (println (##fl= x y))
  (println (##fl= x 0.0))
  (println (##fl= 0.0 y))
  (println (##fl= x -0.0))
  (println (##fl= -0.0 y))
  (println (##fl= x zero))
  (println (##fl= zero y))
  (println (##fl= x negzero))
  (println (##fl= negzero y)))

(define (test-reg+stack a b x y e f)
  (println (##fl< x y))
  (println (##fl< x 0.0))
  (println (##fl< 0.0 y))
  (println (##fl< x -0.0))
  (println (##fl< -0.0 y))
  (println (##fl< x zero))
  (println (##fl< zero y))
  (println (##fl< x negzero))
  (println (##fl< negzero y))

  (println (##fl<= x y))
  (println (##fl<= x 0.0))
  (println (##fl<= 0.0 y))
  (println (##fl<= x -0.0))
  (println (##fl<= -0.0 y))
  (println (##fl<= x zero))
  (println (##fl<= zero y))
  (println (##fl<= x negzero))
  (println (##fl<= negzero y))

  (println (##fl> x y))
  (println (##fl> x 0.0))
  (println (##fl> 0.0 y))
  (println (##fl> x -0.0))
  (println (##fl> -0.0 y))
  (println (##fl> x zero))
  (println (##fl> zero y))
  (println (##fl> x negzero))
  (println (##fl> negzero y))

  (println (##fl>= x y))
  (println (##fl>= x 0.0))
  (println (##fl>= 0.0 y))
  (println (##fl>= x -0.0))
  (println (##fl>= -0.0 y))
  (println (##fl>= x zero))
  (println (##fl>= zero y))
  (println (##fl>= x negzero))
  (println (##fl>= negzero y))

  (println (##fl= x y))
  (println (##fl= x 0.0))
  (println (##fl= 0.0 y))
  (println (##fl= x -0.0))
  (println (##fl= -0.0 y))
  (println (##fl= x zero))
  (println (##fl= zero y))
  (println (##fl= x negzero))
  (println (##fl= negzero y)))


(define (test-stack a x y d e f)
  (println (##fl< x y))
  (println (##fl< x 0.0))
  (println (##fl< 0.0 y))
  (println (##fl< x -0.0))
  (println (##fl< -0.0 y))
  (println (##fl< x zero))
  (println (##fl< zero y))
  (println (##fl< x negzero))
  (println (##fl< negzero y))

  (println (##fl<= x y))
  (println (##fl<= x 0.0))
  (println (##fl<= 0.0 y))
  (println (##fl<= x -0.0))
  (println (##fl<= -0.0 y))
  (println (##fl<= x zero))
  (println (##fl<= zero y))
  (println (##fl<= x negzero))
  (println (##fl<= negzero y))

  (println (##fl> x y))
  (println (##fl> x 0.0))
  (println (##fl> 0.0 y))
  (println (##fl> x -0.0))
  (println (##fl> -0.0 y))
  (println (##fl> x zero))
  (println (##fl> zero y))
  (println (##fl> x negzero))
  (println (##fl> negzero y))

  (println (##fl>= x y))
  (println (##fl>= x 0.0))
  (println (##fl>= 0.0 y))
  (println (##fl>= x -0.0))
  (println (##fl>= -0.0 y))
  (println (##fl>= x zero))
  (println (##fl>= zero y))
  (println (##fl>= x negzero))
  (println (##fl>= negzero y))

  (println (##fl= x y))
  (println (##fl= x 0.0))
  (println (##fl= 0.0 y))
  (println (##fl= x -0.0))
  (println (##fl= -0.0 y))
  (println (##fl= x zero))
  (println (##fl= zero y))
  (println (##fl= x negzero))
  (println (##fl= negzero y)))


(test  0.0  0.0)
(test  0.0 -0.0)
(test -0.0  0.0)
(test -0.0 -0.0)
(test  0.0  1.0)
(test -0.0  1.0)
(test  1.0  0.0)
(test  1.0 -0.0)

(test-reg+stack 9.0 9.0 0.0 0.0 9.0 9.0)
(test-reg+stack 9.0 9.0 0.0 1.0 9.0 9.0)
(test-reg+stack 9.0 9.0 1.0 0.0 9.0 9.0)

(test-stack 9.0 0.0 0.0 9.0 9.0 9.0)
(test-stack 9.0 0.0 1.0 9.0 9.0 9.0)
(test-stack 9.0 1.0 0.0 9.0 9.0 9.0)
