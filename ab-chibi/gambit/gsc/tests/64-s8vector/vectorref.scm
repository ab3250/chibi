(declare (extended-bindings) (not constant-fold) (not safe))

(define v1 (##s8vector -128 -111 0 111 127))
(define v2 (##make-s8vector 10 -128))
(define v3 (##make-s8vector 10 -111))
(define v4 (##make-s8vector 10))
(define v5 (##make-s8vector 10 111))
(define v6 (##make-s8vector 10 127))
(define v7 '#s8(-128 -111 0 111 127))

(define (test v i expected)
  (let ((val (##s8vector-ref v i))) 
    (println (if (##fx= val expected) "good" "bad"))))

(test v1 0 -128)
(test v1 1 -111)
(test v1 2 0)
(test v1 3 111)
(test v1 4 127)

(test v2 9 -128)
(test v3 9 -111)
(test v4 9 0)
(test v5 9 111)
(test v6 9 127)

(test v7 0 -128)
(test v7 1 -111)
(test v7 2 0)
(test v7 3 111)
(test v7 4 127)
