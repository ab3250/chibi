(include "#.scm")

(check-same-behavior ("" "##" "~~lib/gambit/prim/s32vector#.scm")

;; Gambit

(append-s32vectors '(#s32(1) #s32(2) #s32(3)))
(list->s32vector '(1 2 3))
(s32vector-length (make-s32vector 5)) (make-s32vector 5 9)
(subs32vector '#s32(1 2 3 4 5) 1 3)

(let ((x (s32vector 1 2 3 4 5))) (subs32vector-fill! x 1 3 99) x)
(let ((x (s32vector 1 2 3 4)) (y (s32vector 6 7 8 9 0))) (subs32vector-move! x 2 3 y 1) y)
(s32vector) (s32vector 1) (s32vector 1 2) (s32vector 1 2 3)
(s32vector->list '#s32(1 2 3 4 5))
(s32vector-append) (s32vector-append '#s32(1)) (s32vector-append '#s32(1) '#s32(2)) (s32vector-append '#s32(1) '#s32(2) '#s32(3))
(s32vector-copy '#s32(1 2 3 4 5))
(s32vector-copy '#s32(1 2 3 4 5) 1)
(s32vector-copy '#s32(1 2 3 4 5) 1 3)
(let ((x (s32vector 1 2 3 4)) (y (s32vector 6 7 8 9 0))) (s32vector-copy! y 1 x) y)
(let ((x (s32vector 1 2 3 4)) (y (s32vector 6 7 8 9 0))) (s32vector-copy! y 1 x 2) y)
(let ((x (s32vector 1 2 3 4)) (y (s32vector 6 7 8 9 0))) (s32vector-copy! y 1 x 2 3) y)
(let ((x (s32vector 1 2 3 4 5))) (s32vector-fill! x 99) x)
(let ((x (s32vector 1 2 3 4 5))) (s32vector-fill! x 99 1) x)
(let ((x (s32vector 1 2 3 4 5))) (s32vector-fill! x 99 1 3) x)
(s32vector-length '#s32(1 2 3 4 5))
(s32vector-ref '#s32(1 2 3 4 5) 2)
(s32vector-set '#s32(1 2 3 4 5) 2 99)
(let ((x (s32vector 1 2 3 4 5))) (s32vector-set! x 2 99) x)
(let ((x (s32vector 1 2 3 4 5))) (s32vector-shrink! x 3) x)
(s32vector? '#s32(1 2 3)) (s32vector? 123)
)
