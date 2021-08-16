;; If not on Chicken, remove declare, define-type, and : forms
(declare
 (safe-globals)
 (specialize))

(module (srfi 128) ()
  (import scheme)
  (import (only (chicken base) case-lambda error define-record-type
           make-parameter parameterize include infinite? nan? exact-integer?))
  (import (only (chicken module) export))
  (import (only (chicken type) : define-type))
  (import (only (srfi 4) make-u8vector u8vector? u8vector-length u8vector-ref))
  (import (only (srfi 13) string-downcase))

  (export comparator? comparator-ordered? comparator-hashable?)
  (export make-comparator
          make-pair-comparator make-list-comparator make-vector-comparator
          make-eq-comparator make-eqv-comparator make-equal-comparator)
  (export boolean-hash char-hash char-ci-hash
          string-hash string-ci-hash symbol-hash number-hash)
  (export make-default-comparator default-hash comparator-register-default!)
  (export comparator-test-type comparator-check-type comparator-hash)
  (export %salt% hash-bound hash-salt)
  (export =? <? >? <=? >=?)
  (export comparator-if<=>)
  (export comparator-type-test-predicate comparator-equality-predicate
    comparator-ordering-predicate comparator-hash-function)
  (export comparator-max comparator-min
          comparator-max-in-list comparator-min-in-list)
  (export default-comparator boolean-comparator real-comparator
          char-comparator char-ci-comparator
          string-comparator string-ci-comparator
          list-comparator vector-comparator
          eq-comparator eqv-comparator equal-comparator)

  ;; Chicken type definitions
  (define-type +comparator+ (struct comparator))
  (define-type +type-test+ (procedure (*) boolean))
  (define-type +comparison-test+ (procedure (* *) boolean))
  (define-type +hash-code+ fixnum)
  (define-type +hash-function+ (procedure (*) +hash-code+))
  (include "srfi/128/r7rs-shim.scm")
  (include "srfi/128/128-impl.scm")
  (include "srfi/128/default.scm")
  (include "srfi/128/162-impl.scm")  ; must be last

  ;; Chicken type declarations
  (: comparator? (* --> boolean : +comparator+))
  (: comparator-type-test-predicate (+comparator+ --> +type-test+))
  (: comparator-equality-predicate (+comparator+ --> +comparison-test+))
  (: comparator-ordering-predicate (+comparator+ --> +comparison-test+))
  (: comparator-hash-function (+comparator+ --> +hash-function+))
  (: comparator-ordered? (+comparator+ --> boolean))
  (: comparator-hashable? (+comparator+ --> boolean))
  (: make-comparator
   ((or true +type-test+)
    (or true +comparison-test+)
    (or false +comparison-test+)
    (or false +hash-function+)
    --> +comparator+))
  (: comparator-test-type (+comparator+ * --> boolean))
  (: comparator-check-type (+comparator+ * --> true))
  (: comparator-hash (+comparator+ * --> +hash-code+))
  (: binary=? (+comparator+ * * --> boolean))
  (: binary<? (+comparator+ * * --> boolean))
  (: binary>? (+comparator+ * * --> boolean))
  (: binary<=? (+comparator+ * * --> boolean))
  (: binary>=? (+comparator+ * * --> boolean))
  (: =? (+comparator+ * * &rest * * --> boolean))
  (: <? (+comparator+ * * &rest * * --> boolean))
  (: >? (+comparator+ * * &rest * * --> boolean))
  (: <=? (+comparator+ * * &rest * * --> boolean))
  (: >=? (+comparator+ * * &rest * * --> boolean))
  (: boolean<? (boolean boolean --> boolean))
  (: boolean-hash (boolean --> +hash-code+))
  (: char-hash (char --> +hash-code+))
  (: char-ci-hash (char --> +hash-code+))
  (: number-hash (number --> +hash-code+))
  (: complex<? (number number --> boolean)) ;; FIXME
  (: string-ci-hash (string --> +hash-code+))
  (: symbol<? (symbol symbol --> boolean))
  (: symbol-hash (symbol --> +hash-code+))
  (: make-eq-comparator (--> +comparator+))
  (: make-eqv-comparator (--> +comparator+))
  (: make-equal-comparator (--> +comparator+))
  (: limit +hash-code+)
  (: make-pair-comparator (+comparator+ +comparator+ --> +comparator+))
  (: make-pair-type-test (+comparator+ +comparator+ --> +type-test+))
  (: make-pair=? (+comparator+ +comparator+ --> +comparison-test+))
  (: make-pair<? (+comparator+ +comparator+ --> +comparison-test+))
  (: make-hash (+comparator+ +comparator+ --> +hash-function+))
  (: make-list-comparator (+comparator+ +type-test+ +type-test+ (procedure (*) *)  (procedure (*) *) --> +comparator+))
  (: make-vector-comparator (+comparator+ +type-test+ (procedure (*) fixnum) (procedure (* fixnum) *) --> +comparator+))
  (: string-hash (string --> +hash-code+))
  (: comparator-register-default! (+comparator+ -> . *))
  (: default-hash (* --> +hash-code+))
  (: make-default-comparator (--> +comparator+))
)
