#lang r5rs
;; JSON module for Guile

;;; Code:

; (library (json builder)
;   (export scm->json
;           scm->json-string)
;   (import (chezscheme))

;;
;; String builder helpers
;;

(define (unicode->string unicode)
  (format #f "\\u~4,'0x" unicode))

(define (char->unicode-string c)
  (let ((unicode (char->integer c)))
    (if (< unicode 32)
        (unicode->string unicode)
        (string c))))

(define (u8v-2->unicode bv)
  (let ((bv0 (bytevector-u8-ref bv 0))
        (bv1 (bytevector-u8-ref bv 1)))
    (+ (ash (logand bv0 #b00011111) 6)
       (logand bv1 #b00111111))))

(define (u8v-3->unicode bv)
  (let ((bv0 (bytevector-u8-ref bv 0))
        (bv1 (bytevector-u8-ref bv 1))
        (bv2 (bytevector-u8-ref bv 2)))
    (+ (ash (logand bv0 #b00001111) 12)
       (ash (logand bv1 #b00111111) 6)
       (logand bv2 #b00111111))))

(define (build-char-string c)
  (let* ((bv (string->utf8 (string c)))
         (len (bytevector-length bv)))
    (cond
      ;; A single byte UTF-8
      ((eq? len 1) (char->unicode-string c))
      ;; If we have a 2 or 3 byte UTF-8 we need to output it as \uHHHH
      ((or (eq? len 2) (eq? len 3))
       (let ((unicode (if (eq? len 2)
                          (u8v-2->unicode bv)
                          (u8v-3->unicode bv))))
         (unicode->string unicode)))
      ;; Anything else should wrong, hopefully.
      (else (error 'json-invalid "json invalid")))))

;;
;; Object builder functions
;;

(define (build-object-pair p port escape pretty level)
  (display (indent-string pretty level) port)
  (json-build-string (car p) port escape)
  (display " : " port)
  (json-build (cdr p) port escape pretty level))

(define (build-newline port pretty)
  (cond (pretty (newline port))))

(define (indent-string pretty level)
  ;(if pretty (format #f "~v_" (* 4 level)) ""))
  ; fix for chez:
  (if pretty (format #f "~vA" (* 4 level) "") ""))

;;
;; Main builder functions
;;

(define (json-build-null port)
  (display "null" port))

(define (json-build-boolean scm port)
  (display (if scm "true" "false") port))

(define (json-build-number scm port)
  (if (and (rational? scm) (not (integer? scm)))
      (display (number->string (exact->inexact scm)) port)
      (display (number->string scm) port)))

(define (->string x)
  (cond ((char? x) (make-string 1 x))
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        (else x)))

;(define (atom? x)
;  (or (char? x) (number? x) (string? x) (symbol? x)))

(define (json-alist? x)
  (and (pair? x)
       (let loop ((x x))
         (or (null? x)
             (null? (car x))
             (and (pair? (car x)) (atom? (caar x))
                  (loop (cdr x)))))))

(define (json-build-string scm port escape)
  (display "\"" port)
  (display
   (list->string
    (fold-right append '()
                (map
                 (lambda (c)
                   (case c
                     ((#\" #\\) `(#\\ ,c))
                     ((#\backspace) '(#\\ #\b))
                     ((#\page) '(#\\ #\f))
                     ((#\newline) '(#\\ #\n))
                     ((#\return) '(#\\ #\r))
                     ((#\tab) '(#\\ #\t))
                     ((#\/) (if escape `(#\\ ,c) (list c)))
                     (else (string->list (build-char-string c)))))
                 (string->list (->string scm)))))
   port)
  (display "\"" port))

(define (json-build-array scm port escape pretty level)
  (display "(" port)
  (unless (null? scm)
    (json-build (car scm) port escape pretty (+ level 1))
    (for-each (lambda (v)
                (display ", " port)
                (json-build v port escape pretty (+ level 1)))
              (cdr scm)))
  (display ")" port))

(define (json-build-object scm port escape pretty level)
  (build-newline port pretty)
  (format port "~A{" (indent-string pretty level))
  (build-newline port pretty)
  (let ((pairs scm))
    (unless (null? pairs)
      (build-object-pair (car pairs) port escape pretty (+ level 1))
      (for-each (lambda (p)
                  (display "," port)
                  (build-newline port pretty)
                  (build-object-pair p port escape pretty (+ level 1)))
                (cdr pairs))))
  (build-newline port pretty)
  (format port "~A}" (indent-string pretty level)))

(define (hash-table->list hash-table)
  (hash-table-map hash-table (lambda (k v)
                               (cons k v))))

(define (json-build scm port escape pretty level)
  (cond
    ((null? scm) (json-build-null port))
    ((boolean? scm) (json-build-boolean scm port))
    ((number? scm) (json-build-number scm port))
    ((symbol? scm) (json-build-string (symbol->string scm) port escape))
    ((string? scm) (json-build-string scm port escape))
    ((json-alist? scm) (json-build-object scm port escape pretty level))
    ((list? scm) (json-build-array scm port escape pretty level))
    ((hash-table? scm)
     (json-build-object (hash-table->list scm) port escape pretty level))
    (else (error 'json-invalid "json invalid"))))

;;
;; Public procedures
;;

;(define* (scm->json scm
;                    #:optional (port (current-output-port))
;                    #:key (escape #f) (pretty #f))
;  "Creates a JSON document from native. The argument @var{scm} contains
;   the native value of the JSON document. Takes one optional argument,
;   @var{port}, which defaults to the current output port where the JSON
;   document will be written."
;  (json-build scm port escape pretty 0))

(define scm->json
  (case-lambda
    ((scm) (scm->json scm (current-output-port)))
    ((scm port) (scm->json scm port #f #f))
    ((scm port escape pretty) (json-build scm port escape pretty 0))))

;(define* (scm->json-string scm #:key (escape #f) (pretty #f))
;  "Creates a JSON document from native into a string. The argument
;   @var{scm} contains the native value of the JSON document."
;  (call-with-output-string
;    (lambda (p)
;      (scm->json scm p #:escape escape #:pretty pretty))))

(define scm->json-string
  (case-lambda
    ((scm) (scm->json-string scm #f #f))
    ((scm escape pretty) (call-with-string-output-port
                          (lambda (p)
                            (scm->json scm p escape pretty))))))

;;; (json builder) ends here
;(ice-9 rdelim)

;;
;; Parser record and read helpers
;;

;(define-record-type json-parser
;  (make-json-parser port)
;  json-parser?
;  (port json-parser-port))

(define-structure (json-parser port))

(define (parser-peek-char parser)
  (peek-char (json-parser-port parser)))

(define (parser-read-char parser)
  (read-char (json-parser-port parser)))

;(define (parser-read-delimited parser delim handle-delim)
;  (let ((port (json-parser-port parser)))
;    (read-delimited delim port handle-delim)))

;;
;; Number parsing helpers
;;

;; Read + or -. . If something different is found, return empty string.
(define (read-sign parser)
  (let loop ((c (parser-peek-char parser)) (s ""))
    (case c
      ((#\+ #\-)
       (let ((ch (parser-read-char parser)))
         (string-append s (string ch))))
      (else s))))

;; Read digits [0..9]. If something different is found, return empty
;; string.
(define (read-digits parser)
  (let loop ((c (parser-peek-char parser)) (s ""))
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let ((ch (parser-read-char parser)))
         (loop (parser-peek-char parser)
               (string-append s (string ch)))))
      (else s))))

(define (string-null? str)
  (string=? str ""))

(define (read-exp-part parser)
  (let ((c (parser-peek-char parser)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\tab #\vtab #\newline #\return #\space) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ;; We might have the exponential part
      ((#\e #\E)
       (let* ((ch (parser-read-char parser)) ; current char
              (sign (read-sign parser))
              (digits (read-digits parser)))
         ;; If we don't have sign or digits, we have an invalid
         ;; number.
         (if (not (and (string-null? sign)
                       (string-null? digits)))
             (string-append s (string ch) sign digits)
             #f)))
      ;; If we have a character different than e or E, we have an
      ;; invalid number.
      (else #f))))

(define (read-real-part parser)
  (let ((c (parser-peek-char parser)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\tab #\vtab #\newline #\return #\space) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ;; If we read . we might have a real number
      ((#\.)
       (let* ((ch (parser-read-char parser))
              (digits (read-digits parser)))
         ;; If we have digits, try to read the exponential part,
         ;; otherwise we have an invalid number.
         (cond
           ((not (string-null? digits))
            (let ((exp (read-exp-part parser)))
              (cond
                (exp (string-append s (string ch) digits exp))
                (else #f))))
           (else #f))))
      ;; If we have a character different than . we might continue
      ;; processing.
      (else #f))))

(define (read-number parser)
  (let loop ((c (parser-peek-char parser)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\tab #\vtab #\newline #\return #\space) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ((#\-)
       (let ((ch (parser-read-char parser)))
         (loop (parser-peek-char parser)
               (string-append s (string ch)))))
      ((#\0)
       (let* ((ch (parser-read-char parser))
              (tail (or (read-real-part parser)
                        (error 'json-invalid "json invalid" parser))))
         (string-append s
                        (string ch)
                        tail)))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let* ((ch (parser-read-char parser))
              (digit (read-digits parser))
              (tail  (or (read-real-part parser)
                         (read-exp-part parser)
                         (error 'json-invalid "json invalid" parser))))
         (string-append s
                        (string ch)
                        digit
                        tail)))
      (else (error 'json-invalid "json invalid" parser)))))

;;
;; Object parsing helpers
;;

(define (read-pair parser)
  ;; Read string key
  (let ((key (json-read-string parser)))
    (let loop ((c (parser-peek-char parser)))
      (case c
        ;; Skip whitespaces
        ((#\tab #\vtab #\newline #\return #\space)
         (parser-read-char parser)
         (loop (parser-peek-char parser)))
        ;; Skip colon and read value
        ((#\:)
         (parser-read-char parser)
         (cons key (json-read parser)))
        ;; invalid object
        (else (error 'json-invalid "json invalid" parser))))))

(define (read-object parser)
  (let loop ((c (parser-peek-char parser))
             (pairs (make-hashtable equal-hash equal?)))
    (case c
      ;; Skip whitespaces
      ((#\tab #\vtab #\newline #\return #\space)
       (parser-read-char parser)
       (loop (parser-peek-char parser) pairs))
      ;; end of object
      ((#\})
       (parser-read-char parser)
       pairs)
      ;; Read one pair and continue
      ((#\")
       (let ((pair (read-pair parser)))
         (hashtable-set! pairs (car pair) (cdr pair))
         (loop (parser-peek-char parser) pairs)))
      ;; Skip comma and read more pairs
      ((#\,)
       (parser-read-char parser)
       (loop (parser-peek-char parser) pairs))
      ;; invalid object
      (else (error 'json-invalid "json invalid" parser)))))

;;
;; Array parsing helpers
;;

(define (read-array parser)
  (let loop ((c (parser-peek-char parser)) (values '()))
    (case c
      ;; Skip whitespace and comma
      ((#\tab #\vtab #\newline #\return #\space #\,)
       (parser-read-char parser)
       (loop (parser-peek-char parser) values))
      ;; end of array
      ((#\])
       (parser-read-char parser)
       values)
      ;; this can be any json object
      (else
       (let ((value (json-read parser)))
         (loop (parser-peek-char parser)
               (append values (list value))))))))

;;
;; String parsing helpers
;;

(define (expect parser expected)
  (let ((ch (parser-read-char parser)))
    (if (not (char=? ch expected))
        (error 'json-invalid "json invalid" parser)
        ch)))

(define (expect-string parser expected)
  (for-all (lambda (ch) (expect parser ch))
    (string->list expected)))

(define (read-hex-digit parser)
  (let ((c (parser-read-char parser)))
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
            #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) c)
      (else (error 'json-invalid "json invalid" parser)))))

(define (read-control-char parser)
  (let ((c (parser-read-char parser)))
    (case c
      ((#\" #\\ #\/) (string c))
      ((#\b) (string #\backspace))
      ((#\f) (string #\page))
      ((#\n) (string #\newline))
      ((#\r) (string #\return))
      ((#\t) (string #\tab))
      ((#\u)
       (let* ((utf1 (format "~a~a"
                            (read-hex-digit parser)
                            (read-hex-digit parser)))
              (utf2 (format "~a~a"
                            (read-hex-digit parser)
                            (read-hex-digit parser)))
              (vu8 (list (string->number utf1 16)
                         (string->number utf2 16)))
              (utf (u8-list->bytevector vu8)))
         (utf16->string utf 'big)))
      (else #f))))

(define (read-until-quote-or-slash parser)
  (let ((port (json-parser-port parser)))
    (let loop ((buf '()) (c (read-char port)))
      (case c
        ((#\" #\\ #!eof) (cons (list->string (reverse! buf)) c))
(else (loop (cons c buf) (read-char port)))))))

(define (read-string parser)
  ;; Read characters until \ or " are found.
  (let loop ((result "")
             ;(current (parser-read-delimited parser "\\\"" 'split))
             (current (read-until-quote-or-slash parser)))
    (case (cdr current)
      ((#\")
       (string-append result (car current)))
      ((#\\)
       (let ((ch (read-control-char parser)))
         (if ch
             (loop (string-append result (car current) ch)
                   ;(parser-read-delimited parser "\\\"" 'split)
                   (read-until-quote-or-slash parser))
             (error 'json-invalid "json invalid" parser))))
      (else
       (error 'json-invalid "json invalid" parser)))))

;;
;; Main parser functions
;;

(define-syntax json-read-delimited
  (syntax-rules ()
    ((json-read-delimited parser delim read-func)
     (let loop ((c (parser-read-char parser)))
       (case c
         ;; skip whitespace
         ((#\tab #\vtab #\newline #\return #\space) (loop (parser-peek-char parser)))
         ;; read contents
         ((delim) (read-func parser))
         (else (error 'json-invalid "json invalid" parser)))))))

(define (json-read-true parser)
  (expect-string parser "true")
  #t)

(define (json-read-false parser)
  (expect-string parser "false")
  #f)

(define (json-read-null parser)
  (expect-string parser "null")
  '())

(define (json-read-object parser)
  (json-read-delimited parser #\{ read-object))

(define (json-read-array parser)
  (json-read-delimited parser #\( read-array))

(define (json-read-string parser)
  (json-read-delimited parser #\" read-string))

(define (json-read-number parser)
  (string->number (read-number parser)))

(define (json-read parser)
  (let loop ((c (parser-peek-char parser)))
    (cond
      ;;If we reach the end we might have an incomplete document
      ((eof-object? c) (error 'json-invalid "json invalid" parser))
      (else
       (case c
         ;; skip whitespaces
         ((#\tab #\vtab #\newline #\return #\space)
          (parser-read-char parser)
          (loop (parser-peek-char parser)))
         ;; read json values
         ((#\t) (json-read-true parser))
         ((#\f) (json-read-false parser))
         ((#\n) (json-read-null parser))
         ((#\{) (json-read-object parser))
         ((#\[) (json-read-array parser))
         ((#\") (json-read-string parser))
         ;; anything else should be a number
         (else (json-read-number parser)))))))

;;
;; Public procedures
;;

;(define* (json->scm #:optional (port (current-input-port)))
;  "Parse a JSON document into native. Takes one optional argument,
;   @var{port}, which defaults to the current input port from where the JSON
;   document is read."
;  (json-read (make-json-parser port)))

(define json->scm
  (case-lambda
    (() (json->scm (current-input-port)))
    ((port) (json-read (make-json-parser port)))))

;(define* (json-string->scm str)
;  "Parse a JSON document into native. Takes a string argument,
;   @var{str}, that contains the JSON document."
;  (call-with-input-string str (lambda (p) (json->scm p))))

(define (json-string->scm str)
  (json->scm (open-input-string str))))

;;; (json parser) ends here
; (library (json syntax)
;   (export json)
;   (import (chezscheme))

(define (list->hash-table lst)
  (let loop ((table (make-hash-table))
             (lst lst))
    (if (not (null? lst))
        (let* ((pair (car list))
               (key (car pair))
               (value (cdr pair)))
          (hashtable-set! table key value)
          (loop table (cdr list)))
        table)))

(define-syntax json
  (syntax-rules (unquote unquote-splicing array object)
    ((_ (unquote val))
     val)
    ((_ ((unquote-splicing val) . rest))
     (append val (json rest)))
    ((_ (array val . rest))
     (cons (json val) (json rest)))
    ((_ (object key+val ...))
     (list->hash-table
      (json (array key+val ...))))
    ((_ (val . rest))
     (cons (json val) (json rest)))
    ((_ val)
     (quote val))))
)

;;; (json syntax) ends here
