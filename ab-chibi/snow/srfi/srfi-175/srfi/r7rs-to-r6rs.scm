(import (scheme base) (scheme file) (scheme read) (scheme write) (srfi 1))

(cond-expand (chibi (import (chibi show) (chibi show pretty)))
             (else))

(define substitutions
  '((= . fx=?) (< . fx<?) (<= . fx<=?) (> . fx>?) (>= . fx>=?)
    (+ . fx+) (- . fx-) (modulo . fxmod) (exact-integer? . fixnum?)
    (open-binary-input-file . open-file-input-port)
    (open-input-string . open-string-input-port)
    (read-u8 . get-u8)))

(define (substitute form)
  (if (pair? form)
      (cons (substitute (car form))
            (substitute (cdr form)))
      (let ((sub (assoc form substitutions)))
        (if sub (cdr sub) form))))

(define (displayln x)
  (display x)
  (newline))

(define (pretty-print form)
  (cond-expand
    (chibi (show (current-output-port) (pretty form)))
    (else (write form) (newline))))

(define (read-all port)
  (let loop ((forms '()))
    (let ((form (read port)))
      (if (eof-object? form) forms (loop (append forms (list form)))))))

(define (r7rs->r6rs filename)
  (substitute
   (remove (lambda (form) (eqv? 'import (car form)))
           (call-with-input-file filename read-all))))

(define (write-r6rs-file filename . forms)
  (with-output-to-file filename
    (lambda ()
      (displayln "#!r6rs")
      (displayln ";; Automatically generated")
      (displayln ";; Copyright 2019 Lassi Kortela")
      (displayln ";; SPDX-License-Identifier: MIT")
      (for-each (lambda (form) (pretty-print form)) forms))))

(define (write-r6rs-library filename libname exports body)
  (write-r6rs-file
   filename `(library ,libname (export ,@exports) (import (rnrs)) ,@body)))

(define (translate-source-file r7rs-filename r6rs-filename)
  (apply write-r6rs-file r6rs-filename
         (cons '(import (rnrs) (srfi :175))
               (r7rs->r6rs r7rs-filename))))

(define (main)
  (let* ((lib (call-with-input-file "175.sld" read))
         (exports (cdr (assoc 'export (cddr lib))))
         (body (r7rs->r6rs "175.scm")))
    (write-r6rs-library "175.sls" '(srfi :175) exports body)
    (write-r6rs-library "srfi-175.sls" '(srfi srfi-175) exports body) ; Guile
    (translate-source-file "examples.scm" "examples.sps")
    (translate-source-file "tests.scm" "tests.sps")))

(main)
