;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for continuations
;;
(unit-test-start "continuations")

(define (f return)
    (return 2)
      3)
 
(assert/equal (f (lambda (x) x)) 3) 
(assert/equal (call/cc f) 2)

(define (f return)
    (return (+ 1 2 3 (+ 4 5 6)))
      3)

(assert/equal (call/cc f) (+ 1 2 3 4 5 6))
(assert/equal (call-with-current-continuation f) (+ 1 2 3 4 5 6))

(assert/equal (call/cc procedure?) #t)
(assert/equal (call-with-current-continuation procedure?) #t)

(assert/equal
  (call-with-current-continuation
    (lambda (exit)
          (for-each (lambda (x)
                            (if (negative? x)
                                (exit x)))
                   '(54 0 37 -3 245 19))
              #t))
  -3)

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
              (letrec ((r
                (lambda (obj)
                  (cond ((null? obj) 0)
                        ((pair? obj) 
                         (+ (r (cdr obj)) 1))
                        (else (return #f))))))
        (r obj))))))
(define test #f)
(set! test (list-length '(1 2 3 4)))
(assert/equal test 4)
(assert/equal (list-length '(1 2 3 4)) 4)
(assert/equal (list-length '(a b . c)) #f)

(define (test-cont) #f)
(assert/equal (if (call/cc
                    (lambda (c)
                        (set! test-cont c)
                        #f))
                    'true
                    'false)
              'false)
; 
; Following is not a valid test case because in above, (assert/equal) is
; apparently pulled into the continuation. So below, the result of calling
; into the continuation is then compared to false. Weird... but results are
; consistent when run in either husk or chicken scheme.
;
;(assert/equal (test-cont 1)
;              'true)
;
;; The below works fine in huski but causes an infinite loop when compiled.
;; Interestingly, chicken scheme has the same behavior.
(if (husk-interpreter?)
  (begin
    (assert/equal (test-cont #f)
                  'false)
    
    (assert/equal (if (call/cc
                        (lambda (c)
                            (set! test-cont c)
                            #t))
                        'true2)
                  'true2)
    (assert/equal (test-cont #t)
                  'true2)
    
    (assert/equal (begin 1 2 (call/cc
                               (lambda (c)
                                 (set! test-cont c)
                                 3))
                         4)
                  4)
    (assert/equal (test-cont 4) 4)
    (assert/equal (test-cont 3) 4)
    
    (define a #f)
    (set! a (call/cc (lambda (c) (set! test-cont c) 1)))
    (assert/equal a 1)
    (test-cont 2)
    (assert/equal a 2)
    
    ; General function application
    ; Tests from: http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/
    (define handle #f)
    (define test-value #f)
    (set! test-value (+ 2 (call/cc (lambda (k) (set! handle k) 2))))
    (set! test-value (handle 20))
    (assert/equal test-value 22)
    (set! test-value (handle 100))
    (assert/equal test-value 102)
    
    (define test "abcdefg")
    (string-fill! test (call/cc (lambda (k) (set! handle k) #\a)))
    (assert/equal test "aaaaaaa")
    (set! test (handle #\b))
    (assert/equal test "bbbbbbb")
    
    (define test-value #f)
    (define test-cont #f)
    `(a b c ,(call/cc (lambda (k) (set! test-cont k) 'd)) e f g)
    (set! test-value (test-cont 1))
    ; For some reason, returns false.
    ; But this value is confirmed by csi so not worried about it...
    (assert/equal test-value #f)))

; Tests for passing one *or more* values to a continuation
(assert/equal (call-with-values (lambda () (values 1)) (lambda (a) a)) 1)
(assert/equal (call-with-values (lambda () (values 4 5))
                                (lambda (a b) b))
               5)
(assert/equal (call-with-values * -)
               -1)

(unit-test-handler-results)
