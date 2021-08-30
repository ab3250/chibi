(define-library (list-set)
 (export 
  list-set)
 (import
  (scheme base))

(begin
  (define (list-set lst idx val)
    (if (null? lst)
      lst
      (cons
        (if (zero? idx)
          val
          (car lst))
        (list-set (cdr lst) (- idx 1) val))))

 ))
