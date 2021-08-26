(define for-acc (lambda (start end func)
  (let loop ((index start)
             (acc '()))
      (if (> index end)
                      acc   
                      (loop (+ index 1) (func index end acc))))))

