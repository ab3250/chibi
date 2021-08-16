;;
;; A simple unit testing function.
;;
;; This can be simplified using (begin) and (newline), 
;; but neither is supported by huskc at this time.
;;
(define (assert-equal test-id value expression)
  (if (eqv? value expression)
    ((lambda () 
       (display (string-append "Passed: " test-id))
       (display #\newline)))

    ((lambda () 
       (display (string-append "Failed: " test-id))
       (display ", expected [")
       (display value)
       (display "], got [")
       (display expression)
       (display "].")
       (display #\newline)))))

(assert-equal "1.1" 1 1)
(assert-equal "1.2" 2 2)

1
(if 1 2 3)

(define b 1)
(assert-equal "1.3" 1 b)

(define (a x) x)
(assert-equal "1.4" 1234567890 (a 1234567890))

(lambda () 1)

(assert-equal "1.5" 10  (((lambda () +)) 1 2 3 4))
(assert-equal "1.6" 12  ((lambda (a b c) (+ a b c)) 2 4 6))
(assert-equal "1.7" #f  (if #f #t #f))
(assert-equal "1.8" 100 (if ((lambda () #t)) (+ 100) (+ 200)))
(assert-equal "1.9" 200 (if ((lambda () #f)) (+ 100) (+ 200)))
1
2
(assert-equal "1.10" 6 (+ 1 2 3))
(assert-equal "1.11" 14 (+ (+ 1) (+ (+ 2 2)) (+ (+ 3 3) (+ (+ 3)))))
4
1
2
(assert-equal "1.12" 66 (+ 11 22 33))
3
1
2
(assert-equal "1.13" 3 3)
2
(assert-equal "1.14" 15 (+ 1 2 (+ 3 4 5)))
(assert-equal "1.15" 21 (+ 1 2 (+ 3 (+ 4 6) 5)))
(assert-equal "1.16" 16 (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3)))
(assert-equal "1.17" 16 (+ 1 (+ 2) 3 (- 9 2) (/ 9 3)))
(assert-equal "1.18" 15 (+ (+ 2) 3 (- 9 2) (/ 9 3)))
(assert-equal "1.19" 13 (+ 3 (- 9 2) (/ 9 3)))
(assert-equal "1.20" 10 (+ (- 9 2) (/ 9 3)))
(assert-equal "1.21" 3 (+ (/ 9 3))) ; results are OK
(assert-equal "1.22" 16 (if (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3)) 
           (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3))
           (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3))))
(assert-equal "1.23" 3 (if 1 (+ 2 1) (+ 3 1)))
(assert-equal "1.24" 7 (if 1 (+ 2 1 4) 3))
(assert-equal "1.25" 2 (if 1 2 3))
(assert-equal "1.26" 7 (if 1 (+ 2 1 4) 3))
(assert-equal "1.27" 2 (if (+ 1) 2 3))
(assert-equal "1.28" #f (if #f 2 #f))
(assert-equal "1.29" 22 (if (+ 1) 22 3))

(assert-equal "1.30" 3 ((lambda () 1 2 (+ 3) )))
(assert-equal "1.31" 3 ((lambda () 1 2 3 )))
((lambda () 1 2 (+ 3 4)))
(assert-equal "1.32" 3 (if #t (+ ((lambda () 1 2 3) )) 4))
(assert-equal "1.32" 3 (if #t (+ ((lambda () 1 2 3) ))))
(if #f (write "should not see this"))

(assert-equal "1.33" '(1) ((lambda a a) 1))
(assert-equal "1.34" 3 ((lambda (a b . c) (+ a b )) 1 2 3 4 5 6))

(define (f . a) a)
(assert-equal "2.1" '(1 2 3) (f 1 2 3))

(set! f 'test)
(assert-equal "2.11" 'test f)
(set! f 100)
(assert-equal "2.12" 100 f)

(define (f return)
    (return 2)
      3)
 
(assert-equal "3.1" #t (call/cc procedure?))
(assert-equal "3.2" #t (call-with-current-continuation procedure?))
(assert-equal "3.3" 3 (f (lambda (x) x)))
(assert-equal "3.4" 2 (call/cc f))
#|
(define (f return)
    (return (+ 1 2 3 (+ 4 5 6)))
      3)

(assert/equal (call/cc f) (+ 1 2 3 4 5 6))
(assert/equal (call-with-current-continuation f) (+ 1 2 3 4 5 6))


(assert/equal
  (call-with-current-continuation
    (lambda (exit)
          (for-each (lambda (x)
                            (if (negative? x)
                                (exit x)))
                   '(54 0 37 -3 245 19))
              #t))
  -3)
|#

(assert-equal "4.1" 2 (let-syntax () 2))
(assert-equal "4.2" 1 (let ((x 1)) (let-syntax () x)))
(assert-equal "4.3" 3 (let-syntax () 1 2 3))
(assert-equal "4.4" 2 (let-syntax ((my-let (syntax-rules () ((my-let x) (begin x))))) (define x "hello, world") (my-let 2)))

(assert-equal "4.5" 7
 (letrec-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (let ((temp e1))
               (if temp
                   temp
                   (my-or e2 ...)))))))
  (let ((x #f)
        (y 7)
        (temp 8)
        (let odd?)
        (if even?))
    (my-or x
           (let temp)
           (if y)
           y))))
(define f (make-string 3 #\*))
;(define (g) "***")
(string-set! f 0 #\?)
(assert-equal "5.1" "?**" f) 

(define f '(1 2 3 4))
(set-car! f 8)
(assert-equal "5.2" '(8 2 3 4) f)
(define x (list 'a 'b 'c))
(set-cdr! x 4)
(assert-equal "5.3" '(a . 4) x)

(define f '(1 . 2))
(set-car! f "a")
(assert-equal "5.4" '("a" . 2) f)
(define v (make-vector 6))
(assert-equal "5.5" '#(0 1 4 9 16 25) (for-each (lambda (i)
                                       (vector-set! v i (* i i)))
                                      '(0 1 2 3 4 5)))

