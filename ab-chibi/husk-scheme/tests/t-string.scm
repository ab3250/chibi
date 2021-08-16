;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for built-in string forms
;;
(unit-test-start "strings")

; FUTURE: commented out lines indicate issue with Core
;
;         However, since this is a fundamental difference between how a C implementation
;         might work and how our Haskell one works, there may not be an easy or good
;         solution to this problem.
;
;(define (f) (make-string 3 #\*))
(define f (make-string 3 #\*))
(define (g) "***")
(string-set! f 0 #\?)
;(string-set! (f) 0 #\?)          ===>  unspecified
(assert/equal f "?**")
;(string-set! (g) 0 #\?)          ===>  error
;(string-set! (symbol->string 'immutable)
;                          0
;                                       #\?)          ===>  error

(define test "abcdefg")
(assert/equal (string-fill! test #\a)
              "aaaaaaa")

(assert/equal
    (string-map
        (lambda (c)
            (integer->char (+ 1 (char->integer c))))
            "HAL")
    "IBM")

(assert/equal
    (let ((v '()))
        (string-for-each
            (lambda (c) (set! v (cons (char->integer c) v)))
            "abcde")
        v)
    '(101 100 99 98 97))

(unit-test-handler-results)
