;verify random usage
(define-library (knuth-shuffle)
  (export
    knuth-shuffle)
  (import
    (scheme base)
    (srfi 27))
    
(begin
  (define (list-set lst idx val)
    (if (null? lst)
      lst
      (cons
        (if (zero? idx)
          val
          (car lst))
        (list-set (cdr lst) (- idx 1) val))))

  (define (knuth-shuffle lst-org)  
    (let loop ((count (length lst-org)) (lst lst-org))      
      (if (zero? count)
        lst
    (let*   ((j (random-integer count))
      (new-count (- count 1))
            (tmp (list-ref lst new-count))
            (lst2 (list-set lst new-count (list-ref lst j)))
            (lst3 (list-set lst2 j tmp)))	         
            (loop new-count lst3)))))

))

