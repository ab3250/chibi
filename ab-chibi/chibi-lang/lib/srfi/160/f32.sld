
(define-library (srfi 160 f32)
  (export
   make-f32vector
   f32?
   f32vector?
   f32vector-ref
   f32vector-set!
   f32vector-length
   (rename uvector-unfold f32vector-unfold)
   (rename uvector-unfold-right f32vector-unfold-right)
   (rename vector-copy f32vector-copy)
   (rename vector-reverse-copy f32vector-reverse-copy)
   (rename vector-append f32vector-append)
   (rename vector-concatenate f32vector-concatenate)
   (rename vector-append-subvectors f32vector-append-subvectors)
   (rename vector-empty? f32vector-empty?)
   (rename vector= f32vector=)
   (rename vector-take f32vector-take)
   (rename vector-take-right f32vector-take-right)
   (rename vector-drop f32vector-drop)
   (rename vector-drop-right f32vector-drop-right)
   (rename vector-segment f32vector-segment)
   (rename vector-fold f32vector-fold)
   (rename vector-fold-right f32vector-fold-right)
   (rename vector-map f32vector-map)
   (rename vector-map! f32vector-map!)
   (rename vector-for-each f32vector-for-each)
   (rename vector-count f32vector-count)
   (rename vector-cumulate f32vector-cumulate)
   (rename vector-take-while f32vector-take-while)
   (rename vector-take-while-right f32vector-take-while-right)
   (rename vector-drop-while f32vector-drop-while)
   (rename vector-drop-while-right f32vector-drop-while-right)
   (rename vector-index f32vector-index)
   (rename vector-index-right f32vector-index-right)
   (rename vector-skip f32vector-skip)
   (rename vector-skip-right f32vector-skip-right)
   (rename vector-binary-search f32vector-binary-search)
   (rename vector-any f32vector-any)
   (rename vector-every f32vector-every)
   (rename vector-partition f32vector-partition)
   (rename vector-filter f32vector-filter)
   (rename vector-remove f32vector-remove)
   (rename vector-swap! f32vector-swap!)
   (rename vector-fill! f32vector-fill!)
   (rename vector-reverse! f32vector-reverse!)
   (rename vector-copy! f32vector-copy!)
   (rename vector-reverse-copy! f32vector-reverse-copy!)
   (rename reverse-vector->list reverse-f32vector->list)
   (rename reverse-list->vector reverse-list->f32vector)
   (rename uvector->vector f32vector->vector)
   (rename vector->uvector vector->f32vector)
   (rename make-vector-generator make-f32vector-generator)
   (rename write-vector write-f32vector))
  (import (except (scheme base)
                  vector-append vector-copy vector-copy!
                  vector-map vector-for-each)
          (scheme write)
          (srfi 160 base))
  (begin
    (define uvector? f32vector?)
    (define make-uvector make-f32vector)
    (define vector f32vector)
    (define uvector->list f32vector->list)
    (define list->uvector list->f32vector)
    (define uvector-length f32vector-length)
    (define uvector-ref f32vector-ref)
    (define uvector-set! f32vector-set!))
  (include "uvector.scm"))
