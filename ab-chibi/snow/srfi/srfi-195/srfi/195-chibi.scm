;; Copyright (C) Marc Nieper-Wißkirchen (2020).  All Rights Reserved.

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

(define-record-type <box>
  (make-box v)
  box?
  (v unbox box-set!))

(define (box . v*)
  (make-box (%values v*)))

(define (set-box! b . v*)
  (box-set! b (%values v*)))

(define (%values? v)
  (and (pair? v) (eq? *values-tag* (car v))))

(define (box-arity b)
  (let ((v (unbox b)))
    (if (%values? v)
        (length (cdr v))
        1)))

(define (unbox-value b i)
  (let ((v (unbox b)))
    (if (%values? v)
        (list-ref v (+ 1 i))
        v)))

(define (set-box-value! b i obj)
  (let ((v (unbox b)))
    (if (%values? v)
        (list-set! v (+ 1 i) obj)
        (box-set! b obj))))
