(define-library (wslib)
 (export 
  for-accumulator)
 (import
  (scheme base))

(begin
  (define for-acc (lambda (start end func)
    (let loop ((index start)
              (acc '()))
        (if (> index end)
                        acc   
                        (loop (+ index 1) (func index end acc))))))
  (define for-accumulator (lambda (start end acc-initial func )
    (let loop ((index start)
              (acc acc-initial))
        (if (> index end)
                        acc   
                        (loop (+ index 1) (func index end acc))))))
(define func-list (lambda (x end acc)
    (cons (if (< (- x 1)(/ end 2)) "0" "1") acc)))

  (define func-string (lambda (index end acc)
    (if (< (- index 1)(/ end 2)) 
    (string-append "0" acc)
    (string-append "1" acc))))
))
