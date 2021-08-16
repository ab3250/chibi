;; Another SRFI-39 Implementation
;; To improve efficieny, CASE-LAMBDA can be substituted for LAMBDA according to implementations.

(define make-dynamic-bind
  (let ((env '()))
    (cons (lambda (init . conv)
	    (if (null? conv)
		(let ((val init))
		  (letrec ((fp (lambda args
				 (if (null? args)
				     (cond
				      ((assq fp env) => (lambda (pair) (cdr pair)))
				      (else val))
				     (if env
					 (cond
					  ((assq fp env) => (lambda (pair) (set-cdr! pair (car args))))
					  (else (set! val (car args))))
					 (car args))))))
		    fp))
		(let* ((converter (car conv))
		       (val (converter init)))
		  (letrec ((fp (lambda args
				 (if (null? args)
				     (cond
				      ((assq fp env) => (lambda (pair) (cdr pair)))
				      (else val))
				     (if env
					 (cond
					  ((assq fp env) => (lambda (pair) (set-cdr! pair (converter (car args)))))
					  (else (set! val (converter (car args)))))
					 (converter (car args)))))))
		    fp))))
	  (lambda (fls vls body)
	    (let* ((old-env env)
	  	   (new-env
	  	    (dynamic-wind	;for error
	  		(lambda () (set! env #f))
	  		(lambda ()
	  		  (let lp ((fls fls) (vls vls) (new-env old-env))
	  		    (if (null? fls)
	  			new-env
	  			(lp (cdr fls)
	  			    (cdr vls)
	  			    (cons (cons (car fls) ((car fls) (car vls)))
	  				  new-env)))))
	  		(lambda () (set! env old-env)))))
	      (dynamic-wind
	  	  (lambda () (set! env new-env))
	  	  body
	  	  (lambda () (set! env old-env))))))))

(define make-parameter (car make-dynamic-bind))
(define auxi-dynamic (cdr make-dynamic-bind))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((f v) ...) body ...)
     (auxi-dynamic (list f ...) (list v ...) (lambda () body ...)))))

;; (define-macro (parameterize args . body)
;;   `(auxi-dynamic (list ,@(map car args)) (list ,@(map cadr args)) (lambda () ,@body)))

;;; eof
