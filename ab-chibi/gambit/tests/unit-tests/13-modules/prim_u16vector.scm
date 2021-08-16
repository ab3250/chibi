(include "#.scm")

(check-same-behavior ("" "##" "~~lib/gambit/prim/u16vector#.scm")

;; Gambit

(append-u16vectors '(#u16(1) #u16(2) #u16(3)))
(list->u16vector '(1 2 3))
(u16vector-length (make-u16vector 5)) (make-u16vector 5 9)
(subu16vector '#u16(1 2 3 4 5) 1 3)

(let ((x (u16vector 1 2 3 4 5))) (subu16vector-fill! x 1 3 99) x)
(let ((x (u16vector 1 2 3 4)) (y (u16vector 6 7 8 9 0))) (subu16vector-move! x 2 3 y 1) y)
(u16vector) (u16vector 1) (u16vector 1 2) (u16vector 1 2 3)
(u16vector->list '#u16(1 2 3 4 5))
(u16vector-append) (u16vector-append '#u16(1)) (u16vector-append '#u16(1) '#u16(2)) (u16vector-append '#u16(1) '#u16(2) '#u16(3))
(u16vector-copy '#u16(1 2 3 4 5))
(u16vector-copy '#u16(1 2 3 4 5) 1)
(u16vector-copy '#u16(1 2 3 4 5) 1 3)
(let ((x (u16vector 1 2 3 4)) (y (u16vector 6 7 8 9 0))) (u16vector-copy! y 1 x) y)
(let ((x (u16vector 1 2 3 4)) (y (u16vector 6 7 8 9 0))) (u16vector-copy! y 1 x 2) y)
(let ((x (u16vector 1 2 3 4)) (y (u16vector 6 7 8 9 0))) (u16vector-copy! y 1 x 2 3) y)
(let ((x (u16vector 1 2 3 4 5))) (u16vector-fill! x 99) x)
(let ((x (u16vector 1 2 3 4 5))) (u16vector-fill! x 99 1) x)
(let ((x (u16vector 1 2 3 4 5))) (u16vector-fill! x 99 1 3) x)
(u16vector-length '#u16(1 2 3 4 5))
(u16vector-ref '#u16(1 2 3 4 5) 2)
(u16vector-set '#u16(1 2 3 4 5) 2 99)
(let ((x (u16vector 1 2 3 4 5))) (u16vector-set! x 2 99) x)
(let ((x (u16vector 1 2 3 4 5))) (u16vector-shrink! x 3) x)
(u16vector? '#u16(1 2 3)) (u16vector? 123)
)
