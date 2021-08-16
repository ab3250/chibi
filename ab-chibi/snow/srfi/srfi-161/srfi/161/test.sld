;; Copyright (C) Marc Nieper-Wißkirchen (2018).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (srfi 161 test)
  (export run-tests)
  (import (scheme base)
	  (srfi 64)
	  (srfi 161))
  (begin
    (define a (ubox 'a))
    (define b (ubox 'b))
    (define c (ubox 'c))
    (define d (ubox 'd))
    (define e (ubox 'e))
    (define f (ubox 'f))

    (define (run-tests)
      (test-begin "Unifiable Boxes")

      (test-assert (ubox? (ubox 'g)))

      (test-assert (not (ubox? (vector 'h))))

      (test-assert (not (ubox=? a b)))

      (test-eq
	  'a
	(ubox-ref a))

      (ubox-link! a b)
      (ubox-union! a c)
      (ubox-unify! cons d e)
      (ubox-link! b f)

      (test-assert (ubox=? a b))
      (test-assert (ubox=? b c))
      (test-assert (ubox=? c f))
      (test-assert (ubox=? a f))
      (test-assert (ubox=? d e))
      (test-assert (not (ubox=? a e)))

      (test-eq (ubox-ref a) 'f)
      (test-equal (ubox-ref d) '(d . e))

      (ubox-set! b 'i)

      (test-eq (ubox-ref a) 'i)

      (ubox-link! a e)

      (test-assert (ubox=? c e))

      (test-end))))
