;; This is the "fast" linear congruential pseudo-random number
;; generator `ranqd1' from chapter 7 of Numerical Recipes.  Note that
;; because Scheme systems generally offer native support for "big"
;; integers, one has to use `modulo' in the Scheme code to emulate the
;; behavior of 32-bit Fortran and C systems.
;;
;; See http://lib-www.lanl.gov/numerical/bookfpdf/f7-1.pdf for details.
;;
;; Emilio C. Lopes <eclig@gmx.net>, 2005-01-04.

(define (gen-ranqd1 seed)
  (let ((random seed)
        (2**32 #x100000000))
    (lambda ()
      (set! random (modulo (+ (* random 1664525) 1013904223) 2**32))
      random)))
