; Copyright (c) 1993 by Richard Kelsey and Jonathan Rees.  See file COPYING.

; The usual macros (from Scheme 48)

(define (for-each-usual-macro proc)
  (proc 'and rewrite/and)
  (proc 'case rewrite/case)
  (proc 'cond rewrite/cond)
  (proc 'delay rewrite/delay)
  (proc 'do rewrite/do)
  (proc 'let rewrite/let)
  (proc 'let* rewrite/let*)
  (proc 'letrec rewrite/letrec)
  (proc 'or rewrite/or)
  (proc 'quasiquote rewrite/quasiquote))


(define rewrite/and
  (lambda (rename compare . conjuncts)
    (cond ((null? conjuncts) `#t)
          ((null? (cdr conjuncts)) (car conjuncts))
          (else `(,(rename 'if) ,(car conjuncts)
		     (,(rename 'and) ,@(cdr conjuncts))
		     ;; Scheme 48 bootstrapping constraints forbid
		     ;; embedding #F in quoted structure.
		     ,#f)))))

(define rewrite/case
  (lambda (rename compare . (key . clauses))
    (let ((temp (rename 'temp))
	  (%eqv? (rename 'eq?))
	  (%memv (rename 'memv))
	  (%quote (rename 'quote)))
      `(,(rename 'let) ((,temp ,key))
	 (,(rename 'cond) ,@(map (lambda (clause)
			`(,(cond ((compare (car clause) (rename 'else))
				  (car clause))
				 ((null? (car clause))
				  #f)
				 ((null? (cdar clause)) ;+++
				  `(,%eqv? ,temp (,%quote ,(caar clause))))
				 (else
				  `(,%memv ,temp (,%quote ,(car clause)))))
			  ,@(cdr clause)))
		      clauses))))))

(define rewrite/cond
  (lambda (rename compare . clauses)
    (cond ((null? clauses) `(,(rename 'unspecific)))
          ((null? (cdar clauses))
           `(,(rename 'or) ,(caar clauses)
                (,(rename 'cond) ,@(cdr clauses))))
          ((compare (caar clauses) (rename 'else))
           `(,(rename 'begin) ,@(cdar clauses)))
          ((compare (cadr (car clauses)) (rename '=>))
	   (let ((temp (rename 'temp)))
	     `(,(rename 'let) ((,temp ,(car (car clauses))))
		(,(rename 'if) ,temp
		    (,(caddr (car clauses)) ,temp)
		    (,(rename 'cond) ,@(cdr clauses))))))
          (else `(,(rename 'if) ,(caar clauses)
                     (,(rename 'begin) ,@(cdar clauses))
                     (,(rename 'cond) ,@(cdr clauses)))))))

(define rewrite/delay
  (lambda (rename compare . (thing))
    `(,(rename 'make-promise) (,(rename 'lambda) () ,thing))))

(define rewrite/do
  (lambda (rename compare . (specs end . body))
    (let ((%loop (rename 'loop))
	  (%letrec (rename 'letrec))
	  (%lambda (rename 'lambda))
	  (%cond (rename 'cond)))
      `(,%letrec ((,%loop
		   (,%lambda ,(map car specs)
			     (,%cond ,end
				     (else ,@body
					   (,%loop
					    ,@(map (lambda (y)
						     (if (null? (cddr y))
							 (car y)
							 (caddr y)))
						   specs)))))))
		 (,%loop ,@(map cadr specs))))))

(define rewrite/let
  (lambda (rename compare . (specs . body))
    (cond ((symbol? specs)
           (let ((tag specs)
                 (specs (car body))
                 (body (cdr body))
		 (%letrec (rename 'letrec))
		 (%lambda (rename 'lambda)))
             `(,%letrec ((,tag (,%lambda ,(map car specs) ,@body)))
			(,tag ,@(map cadr specs)))))
          (else
           `((,(rename 'lambda) ,(map car specs) ,@body)
             ,@(map cadr specs))))))

(define rewrite/let*
  (lambda (rename compare . (specs . body))
    (if (or (null? specs)
            (null? (cdr specs)))
        `(,(rename 'let) ,specs ,@body)
        `(,(rename 'let) (,(car specs))
			 (,(rename 'let*) ,(cdr specs) ,@body)))))

; (reverse specs) in order to try to catch unportabilities

(define rewrite/letrec
  (lambda (rename compare . (specs . body))
    (let ((unassigned `(,(rename 'unassigned)))
	  (%lambda (rename 'lambda))
	  (%set! (rename 'set!)))
      `((,%lambda ,(map car specs)
		  ,@(map (lambda (spec) `(,%set! ,@spec)) (reverse specs))
		  ((,%lambda () ,@body)))  ;allow internal defines
	,@(map (lambda (spec)
		 spec			;ignored
		 unassigned)
	       specs)))))

(define rewrite/or
  (lambda (rename compare . disjuncts)
    (cond ((null? disjuncts) #f)  ;not '#f
          ((null? (cdr disjuncts)) (car disjuncts))
          (else (let ((temp (rename 'temp)))
		  `(,(rename 'let) ((,temp ,(car disjuncts)))
		     (,(rename 'if) ,temp
			 ,temp
			 (,(rename 'or) ,@(cdr disjuncts)))))))))


; Quasiquote

(define rewrite/quasiquote
  (lambda (rename compare . (x))

    (define %quote (rename 'quote))
    (define %quasiquote (rename 'quasiquote))
    (define %unquote (rename 'unquote))
    (define %unquote-splicing (rename 'unquote-splicing))
    (define %append (rename 'append))
    (define %cons (rename 'cons))
    (define %list->vector (rename 'list->vector))

    (define (expand-quasiquote x level)
      (descend-quasiquote x level finalize-quasiquote))

    (define (finalize-quasiquote mode arg)
      (cond ((eq? mode 'quote) `(,%quote ,arg))
	    ((eq? mode 'unquote) arg)
	    ((eq? mode 'unquote-splicing)
	     (syntax-error ",@ in invalid context" arg))
	    (else `(,mode ,@arg))))

    (define (descend-quasiquote x level return)
      (cond ((vector? x)
	     (descend-quasiquote-vector x level return))
	    ((not (pair? x))
	     (return 'quote x))
	    ((interesting-to-quasiquote? x %quasiquote)
	     (descend-quasiquote-pair x (+ level 1) return))
	    ((interesting-to-quasiquote? x %unquote)
	     (cond ((= level 0)
		    (return 'unquote (cadr x)))
		   (else
		    (descend-quasiquote-pair x (- level 1) return))))
	    ((interesting-to-quasiquote? x %unquote-splicing)
	     (cond ((= level 0)
		    (return 'unquote-splicing (cadr x)))
		   (else
		    (descend-quasiquote-pair x (- level 1) return))))
	    (else
	     (descend-quasiquote-pair x level return))))

    (define (descend-quasiquote-pair x level return)
      (descend-quasiquote (car x) level
	(lambda (car-mode car-arg)
	  (descend-quasiquote (cdr x) level
	    (lambda (cdr-mode cdr-arg)
	      (cond ((and (eq? car-mode 'quote) (eq? cdr-mode 'quote))
		     (return 'quote x))
		    ((eq? car-mode 'unquote-splicing)
		     ;; (,@mumble ...)
		     (cond ((and (eq? cdr-mode 'quote) (null? cdr-arg))
			    (return 'unquote
				    car-arg))
			   (else
			    (return %append
				    (list car-arg (finalize-quasiquote
						     cdr-mode cdr-arg))))))
		    (else
		     (return %cons
			     (list (finalize-quasiquote car-mode car-arg)
				   (finalize-quasiquote cdr-mode cdr-arg))))))))))

    (define (descend-quasiquote-vector x level return)
      (descend-quasiquote (vector->list x) level
	(lambda (mode arg)
	  (case mode
	    ((quote) (return 'quote x))
	    (else (return %list->vector
			  (list (finalize-quasiquote mode arg))))))))

    (define (interesting-to-quasiquote? x marker)
      (and (pair? x) (compare (car x) marker)))

    (expand-quasiquote x 0)))
