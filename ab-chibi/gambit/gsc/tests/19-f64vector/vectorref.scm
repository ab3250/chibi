(declare (extended-bindings) (not constant-fold) (not safe))

(define v1 (##f64vector 0.0 -0.0 1.5 -2.5))
(define v2 (##make-f64vector 10))
(define v3 (##make-f64vector 10 -0.0))
(define v4 (##make-f64vector 10 1.5))
(define v5 (##make-f64vector 10 -2.5))
(define v6 '#f64(0.0 -0.0 1.5 -2.5))

(define (test v i expected)
  (let ((val (##f64vector-ref v i))) 
    (println (if (##fleqv? val expected) "good" "bad"))))

(test v1 0 0.0)
(test v1 1 -0.0)
(test v1 2 1.5)
(test v1 3 -2.5)

(test v2 9 0.0)
(test v3 9 -0.0)
(test v4 9 1.5)
(test v5 9 -2.5)

(test v6 0 0.0)
(test v6 1 -0.0)
(test v6 2 1.5)
(test v6 3 -2.5)
