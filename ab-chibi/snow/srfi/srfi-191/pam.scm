;;;; An implementation of SRFI 191 on top of SRFI 102

(define (procedure-arity-mask proc)
  ;; Get either an integer, an arity-at-least object, or a list of them
  (let ((raw-arity (procedure-arity proc)))
    ; loop through list of integers and arity-at-least objects
    (let loop ((arity (if (pair? raw-arity) raw-arity (list raw-arity)))
               (n 0)
               (more-args +inf.0))
      ;(print "arity = " arity " n = " n " more-args = " more-args)
      (cond
        ;; done, no more-args
        ((and (null? arity) (= +inf.0 more-args))
         n)
        ;; done with more-args, need to merge them in
        ((null? arity)
         (bitwise-ior n (- (expt 2 more-args))))
        ;; integer means "allow exactly n arguments"
        ((exact-integer? (car arity))
         (loop
           (cdr arity)
           (+ n (expt 2 (car arity)))
           more-args))
        ;; pull the value out of the arity-at-least
        ((arity-at-least? (car arity))
         (let ((at-least (arity-at-least-value (car arity))))
           (loop
             (cdr arity)
             n
             ;; save only the smallest arity-at-least value
             (if (< at-least more-args) at-least more-args))))))))
