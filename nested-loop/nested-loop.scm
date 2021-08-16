(import (scheme base)(scheme red))


(define for (lambda (start end func)
  (let loop ((index start)
             (acc '()))
      (if (> index end)
                      acc   
                      (loop (+ index 1) (func index end acc))))))

(define (func x end acc)
  (cons (if (< (- x 1)(/ end 2)) "0" "1" ) acc))



(define (main args)

(display (for 1 20 func))



)
; (nested-loop row 0 8 col 0 8
; (display row)(display col)(newline)
; )





; (define fill-array(lambda (start1 end1 start2 end2 arry)
;   (let loop1 ((index1 start1)(arry1 arry))
;     (if (< index1 end1)
;       (let loop2 ((index2 start2)(arry2 arry1))
;         (if(< index2 end2)
;           (loop2 (+ index2 1)(append arry2 (list(string-append (padn index1 2) (padn index2 2)))))
;           (loop1 (+ index1 1) arry2)))
;       arry1))))