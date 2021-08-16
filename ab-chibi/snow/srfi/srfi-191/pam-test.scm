;;;; A test of pam.scm, using a fake representation of procedures
;;; that will work on any R7RS Scheme that supports bitwise-ior.
;;; It does not require support for SRFI 102.

;;; Fake procedures are lists where car is a symbolic name, the cdr
;;; is a list of integers representing valid exact argument counts and
;;; lists of a single integer representing a N-or-more argument count.
;;; Thus (foo 1 3 (5)) means that 1 argument, 3 arguments, or 5 or more
;;; arguments are all valid arities for foo.

(import (scheme base))
(import (only (srfi 151) bitwise-ior))

;;; Fake implementation of SRFI 102
(define procedure-arity cdr)
(define arity-at-least? pair?)
(define arity-at-least-value car)

;;; Test harness
(import (scheme write))
(define (print . x)
  (print* x))

(define (print* x)
  (if (null? x)
    (newline)
    (begin
      (display (car x))
      (print* (cdr x)))))

(define-syntax test
  (syntax-rules ()
    ((test expected expr)
     (let ((got expr))
       (print 'expr ":")
       (unless (= expected got)
         (print "Expected " expected ", got " got))))))

(include "pam.scm")

;;; Test cases using fake procedures.

(test 1 (procedure-arity-mask '(newline 0)))
(test 2 (procedure-arity-mask '(car 1)))
(test 4 (procedure-arity-mask '(cons 2)))
(test -1 (procedure-arity-mask '(plus (0))))
(test -2 (procedure-arity-mask '(minus (1))))
(test 42 (procedure-arity-mask '(case-lambda-1 1 3 5)))
(test -43 (procedure-arity-mask '(case-lambda-2 0 2 4 (6))))
(test -1 (procedure-arity-mask '(bizarre 1 7 2 (5) (2) 0)))
