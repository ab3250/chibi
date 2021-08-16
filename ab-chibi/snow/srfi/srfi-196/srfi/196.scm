;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define (natural? x)
  (and (integer? x) (not (negative? x))))

(define unspecified (if #f #f))

(define-record-type <range>
  (raw-range comparator lower-bound length indexer)
  range?
  (comparator range-element-comparator)
  (lower-bound range-lower-bound)
  (length range-length)
  (indexer range-indexer))

;; Returns an empty range which is otherwise identical to r.
(define (%empty-range-from r)
  (raw-range (range-element-comparator r)
             (range-lower-bound r)
             0
             (range-indexer r)))

(define (%range-type? r obj)
  ((comparator-type-test-predicate (range-element-comparator r)) obj))

(define (%range-valid-index? r index)
  (and (natural? index) (< index (range-length r))))

;;;; Constructors

;; The primary range constructor does some extra consistency checking.
(define (range comparator lower-bound length indexer)
  (assume (comparator? comparator))
  (assume (natural? length))
  (assume (procedure? indexer))
  (assume ((comparator-type-test-predicate comparator) lower-bound))
  (raw-range comparator lower-bound length indexer))

(define numeric-range
  (case-lambda
    ((start end) (numeric-range start end 1))
    ((start end step)
     (let ((len (/ (- end start) step)))
       (assume (and (natural? len)
                    (or (= start end)
                        ((if (< start end) < >) (+ start (* (- len 1) step))
                                                end)))
               "numeric-range: computed length is invalid")
       (raw-range real-comparator
                  start
                  (exact len)
                  (lambda (b n) (+ b (* n step))))))))

;;;; Predicates

(define (range-contains? r value)
  (assume (range? r))
  (assume (%range-type? r value))
  (let ((cmp (range-element-comparator r)))
    (and (range-index (lambda (x) (=? cmp x value)) r) #t)))

(define (range-includes? r value)
  (assume (range? r))
  (assume (%range-type? r value))
  (let ((cmp (range-element-comparator r)))
    (and (>=? cmp value (range-start r))
         (<=? cmp value (range-end r)))))

(define (range-empty? r)
  (assume (range? r))
  (<= (range-length r) 0))

;;;; Accessors

(define (range-ref r index)
  (assume (%range-valid-index? r index) "range-ref: invalid index")
  ((range-indexer r) (range-lower-bound r) index))

;; A portable implementation can't rely on inlining, but it
;; can rely on macros.
(define-syntax %range-ref-no-check
  (syntax-rules ()
    ((_ r index)
     ((range-indexer r) (range-lower-bound r) index))))

(define (range-start r) (%range-ref-no-check r 0))

(define (range-end r) (%range-ref-no-check r (- (range-length r) 1)))

;;;; Iteration

(define (range-split-at r index)
  (assume (range? r))
  (let ((cmp (range-element-comparator r))
        (indexer (range-indexer r)))
    (assume (%range-valid-index? r index) "range-split: invalid index")
    (values
     (raw-range cmp (range-start r) index indexer)
     (raw-range cmp
                (%range-ref-no-check r index)
                (- (range-length r) index)
                indexer))))

(define (subrange r start end)
  (assume (range? r))
  (assume (%range-valid-index? r start) "subrange: invalid start index")
  (assume (and (natural? end) (> end start) (<= end (range-length r)))
          "subrange: invalid end index")
  (if (and (zero? start) (= end (range-length r)))
      r
      (raw-range (range-element-comparator r)
                 (%range-ref-no-check r start)
                 (- end start)
                 (range-indexer r))))

(define (range-take r count)
  (assume (range? r))
  (assume (%range-valid-index? r count) "range-take: invalid count")
  (if (zero? count)
      (%empty-range-from r)
      (raw-range (range-element-comparator r)
                 (range-lower-bound r)
                 count
                 (range-indexer r))))

(define (range-take-right r count)
  (assume (range? r))
  (assume (%range-valid-index? r count)
          "range-take-right: invalid count")
  (if (zero? count)
      (%empty-range-from r)
      (raw-range (range-element-comparator r)
                 (%range-ref-no-check r (- (range-length r) count))
                 count
                 (range-indexer r))))

(define (range-drop r count)
  (assume (range? r))
  (assume (%range-valid-index? r count) "range-drop: invalid count")
  (if (zero? count)
      r
      (raw-range (range-element-comparator r)
                 (%range-ref-no-check r count)
                 (- (range-length r) count)
                 (range-indexer r))))

(define (range-drop-right r count)
  (assume (range? r))
  (assume (%range-valid-index? r count) "range-drop: invalid count")
  (if (zero? count)
      r
      (raw-range (range-element-comparator r)
                 (range-lower-bound r)
                 (- (range-length r) count)
                 (range-indexer r))))

(define (range-count pred r)
  (assume (procedure? pred))
  (range-fold (lambda (x c) (if (pred x) (+ c 1) c)) 0 r))

(define (range-any pred r)
  (assume (procedure? pred))
  (range-fold (lambda (x last) (or (pred x) last)) #f r))

(define (range-every pred r)
  (assume (procedure? pred))
  (call-with-current-continuation
   (lambda (return)
     (range-fold (lambda (x _) (or (pred x) (return #f))) #t r))))

(define (range-map->list proc r)
  (assume (procedure? proc))
  (range-fold-right (lambda (elem xs) (cons (proc elem) xs))
                    '()
                    r))

(define (range-for-each proc r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0))
      (if (>= i len)
          unspecified
          (begin
           (proc (%range-ref-no-check r i))
           (lp (+ i 1)))))))

(define (range-fold proc nil r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0) (acc nil))
      (if (>= i len)
          acc
          (lp (+ i 1) (proc (%range-ref-no-check r i) acc))))))

(define (range-fold-right proc nil r)
  (assume (procedure? proc))
  (assume (range? r))
  (let ((len (range-length r)))
    (let rec ((i 0))
      (if (>= i len)
          nil
          (proc (%range-ref-no-check r i) (rec (+ i 1)))))))

(define (range-filter->list pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (range-fold-right (lambda (x xs)
                      (if (pred x) (cons x xs) xs))
                    '()
                    r))

(define (range-remove->list pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (range-fold-right (lambda (x xs)
                      (if (pred x) xs (cons x xs)))
                    '()
                    r))

(define (range-reverse r)
  (assume (range? r))
  (raw-range (range-element-comparator r)
             (range-lower-bound r)
             (range-length r)
             (lambda (b n)
               ((range-indexer r) b (- (range-length r) 1 n)))))

;;;; Searching

(define (range-index pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (let ((len (range-length r)))
    (let lp ((i 0))
      (cond ((>= i len) #f)
            ((pred (%range-ref-no-check r i)) i)
            (else (lp (+ i 1)))))))

(define (range-index-right pred r)
  (assume (procedure? pred))
  (assume (range? r))
  (let lp ((i (- (range-length r) 1)))
    (cond ((< i 0) #f)
          ((pred (%range-ref-no-check r i)) i)
          (else (lp (- i 1))))))

(define (range-take-while pred r)
  (let ((count (range-index (lambda (x) (not (pred x))) r)))
    (if count (range-take r count) r)))

(define (range-take-while-right pred r)
  (let ((idx (range-index-right (lambda (x) (not (pred x))) r)))
    (if idx (range-take-right r (- (range-length r) 1 idx)) r)))

(define (range-drop-while pred r)
  (let ((count (range-index (lambda (x) (not (pred x))) r)))
    (if count (range-drop r count) (%empty-range-from r))))

(define (range-drop-while-right pred r)
  (let ((idx (range-index-right (lambda (x) (not (pred x))) r)))
    (if idx
        (range-drop-right r (- (range-length r) 1 idx))
        (%empty-range-from r))))

;;;; Conversion

(define (range->list r)
  (assume (range? r))
  (range-fold-right cons '() r))

(define (range->vector r)
  (assume (range? r))
  (let* ((len (range-length r))
         (vec (make-vector len)))
    (let lp ((i 0))
      (if (= i len)
          vec
          (begin
           (vector-set! vec i (%range-ref-no-check r i))
           (lp (+ i 1)))))))

(define (range->generator r)
  (assume (range? r))
  (let ((i 0) (len (range-length r)))
    (lambda ()
      (if (>= i len)
          (eof-object)
          (begin
           (let ((v (%range-ref-no-check r i)))
             (set! i (+ i 1))
             v))))))
