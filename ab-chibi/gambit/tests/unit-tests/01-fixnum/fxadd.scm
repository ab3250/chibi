(include "#.scm")

(check-eqv? (##fx+ 11 33)   44)
(check-eqv? (##fx+ 11 -11)   0)
(check-eqv? (##fx+ 11 -33) -22)
(check-eqv? (##fx+ -11 33)  22)

(check-eqv? (##fx+) 0)
(check-eqv? (##fx+ 11) 11)
(check-eqv? (##fx+ 11 22) 33)
(check-eqv? (##fx+ 11 22 33) 66)
(check-eqv? (##fx+ 11 22 33 44) 110)

(check-eqv? (fx+ 11 33)   44)
(check-eqv? (fx+ 11 -11)   0)
(check-eqv? (fx+ 11 -33) -22)
(check-eqv? (fx+ -11 33)  22)

(check-eqv? (fx+) 0)
(check-eqv? (fx+ 11) 11)
(check-eqv? (fx+ 11 22) 33)
(check-eqv? (fx+ 11 22 33) 66)
(check-eqv? (fx+ 11 22 33 44) 110)

(check-tail-exn fixnum-overflow-exception? (lambda () (fx+ ##max-fixnum 1)))
(check-tail-exn fixnum-overflow-exception? (lambda () (fx+ 0 ##max-fixnum 1 0)))
(check-tail-exn fixnum-overflow-exception? (lambda () (fx+ ##min-fixnum -1)))
(check-tail-exn fixnum-overflow-exception? (lambda () (fx+ 0 ##min-fixnum -1 0)))

(check-tail-exn type-exception? (lambda () (fx+ 0.0)))
(check-tail-exn type-exception? (lambda () (fx+ 0.5)))
(check-tail-exn type-exception? (lambda () (fx+ 0.5 9)))
(check-tail-exn type-exception? (lambda () (fx+ 9 0.5)))
(check-tail-exn type-exception? (lambda () (fx+ 0.5 3 9)))
(check-tail-exn type-exception? (lambda () (fx+ 3 0.5 9)))
(check-tail-exn type-exception? (lambda () (fx+ 3 9 0.5)))
