(include "#.scm")

(check-same-behavior ("" "##" "~~lib/gambit/prim/s16vector#.scm")

;; Gambit

(append-s16vectors '(#s16(1) #s16(2) #s16(3)))
(list->s16vector '(1 2 3))
(s16vector-length (make-s16vector 5)) (make-s16vector 5 9)
(subs16vector '#s16(1 2 3 4 5) 1 3)

(let ((x (s16vector 1 2 3 4 5))) (subs16vector-fill! x 1 3 99) x)
(let ((x (s16vector 1 2 3 4)) (y (s16vector 6 7 8 9 0))) (subs16vector-move! x 2 3 y 1) y)
(s16vector) (s16vector 1) (s16vector 1 2) (s16vector 1 2 3)
(s16vector->list '#s16(1 2 3 4 5))
(s16vector-append) (s16vector-append '#s16(1)) (s16vector-append '#s16(1) '#s16(2)) (s16vector-append '#s16(1) '#s16(2) '#s16(3))
(s16vector-copy '#s16(1 2 3 4 5))
(s16vector-copy '#s16(1 2 3 4 5) 1)
(s16vector-copy '#s16(1 2 3 4 5) 1 3)
(let ((x (s16vector 1 2 3 4)) (y (s16vector 6 7 8 9 0))) (s16vector-copy! y 1 x) y)
(let ((x (s16vector 1 2 3 4)) (y (s16vector 6 7 8 9 0))) (s16vector-copy! y 1 x 2) y)
(let ((x (s16vector 1 2 3 4)) (y (s16vector 6 7 8 9 0))) (s16vector-copy! y 1 x 2 3) y)
(let ((x (s16vector 1 2 3 4 5))) (s16vector-fill! x 99) x)
(let ((x (s16vector 1 2 3 4 5))) (s16vector-fill! x 99 1) x)
(let ((x (s16vector 1 2 3 4 5))) (s16vector-fill! x 99 1 3) x)
(s16vector-length '#s16(1 2 3 4 5))
(s16vector-ref '#s16(1 2 3 4 5) 2)
(s16vector-set '#s16(1 2 3 4 5) 2 99)
(let ((x (s16vector 1 2 3 4 5))) (s16vector-set! x 2 99) x)
(let ((x (s16vector 1 2 3 4 5))) (s16vector-shrink! x 3) x)
(s16vector? '#s16(1 2 3)) (s16vector? 123)
)
