;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for case form
;;
(unit-test-start "case")

; Tests from spec
(assert/equal (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))
              'composite)

(assert/equal (case (car '(c d))
                ((a e i o u) 'vowel)
                ((w y) 'semivowel)
                (else 'consonant))
              'consonant)

; This is no longer a valid test case.
; Per R5RS the result is unspecified; in husk 
; it is a value we cannot test for directly.
;(assert/equal (case (car '(c d)) 
;                ((a) 'a)
;                ((b) 'b))
;              #f)

; Misc test cases
(assert/equal (case (* 2 3) ((6) '(#t)) (else #f)) 
			  '(#t))

(assert/equal (case (* 2 3) ((6) '(#t)) (else #f))
			  '(#t))

(assert/equal (case (* 2 3) ((6) 6) (else #f))
			  6)

(assert/equal (case (* 2 3) ((8 9 10 2 3 4 5 1 3 6) #t) (else #f))
			  #t)

(assert/equal (case (* 2 3) ((4) #f) (else '(#t)))
			  '(#t))

(assert/equal (case (* 2 3) ((4 5 7 9 4 2 10) #f) (else '(#t)))
			  '(#t))

(assert/equal (case (* 2 3) (else #t))
			  #t)

; First clause has no datums
(assert/equal (case 1 (() 'test) (else 'pass))
              'pass)

(assert/equal
    (case (car '(c d))
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else => (lambda (x) x)))
    'c)

(unit-test-handler-results)
