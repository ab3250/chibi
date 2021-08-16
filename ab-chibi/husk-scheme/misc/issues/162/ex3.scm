(import (scheme base)
        (scheme write)
        (husk pretty-print)
        (pattern-match-lambda))

;(define-syntax exam
;  (syntax-rules ()
;    ((_ form expect)
;     (begin
;       (write 'form)
;       (display " ... ")
;       (display (if (equal? form expect) 'ok 'ng))
;       (newline)))))
;
;(define fact
;  (pattern-match-lambda ()
;   ((0) 1)
;   ((n) (* n (fact (- n 1))))))
;
;(exam (fact 5) 120)
;
; (define example
;   (pattern-match-lambda ()
;     ((x y z) (list 'case1 x y z))
;     ((x (y z)) (list 'case2 x y z))
;     (((x y) z) (list 'case3 x y z))
;     (else 'case3)))
; 
;;(write (expand (example 1 2 3)))
;
;(exam (example 1 2 3) '(case1 1 2 3))
;(exam (example 4 '(5 6)) '(case2 4 5 6))
;(exam (example '(7 8) 9) '(case3 7 8 9))
;(exam (example 10 11 12 13) 'case3)
; 
;(define example2
;  (pattern-match-lambda (foo bar baz)
;    ((foo 1) 'foo-case-1)
;    ((foo 2) 'foo-case-2)
;    ((foo (x #(y z))) (list 'foo-case x y z))
;    ((bar x) (list 'bar-case x))
;    ((baz x) (list 'baz-case x))
;    (else 'else-case)))
;
;(exam (example2 'foo 1) 'foo-case-1)
;(exam (example2 'foo '(1 #(2 3))) '(foo-case 1 2 3))
;(exam (example2 'foo 2) 'foo-case-2)
;(exam (example2 'baz 4) '(baz-case 4))
;
;; TODO:
;;; Underbar is placeholder
;(write (pretty-print (expand
;  (pattern-match-lambda ()
;    ((_) 'arity1)
;    ((_ _) 'arity2)
;    ((_ _ _) 'arity3)))))

;(write (pretty-print (expand
;(lambda lst
;  (%pattern-match-lambda
;    ()
;    lst
;    ((_) 'arity1)
;    ((_ _) 'arity2)
;    ((_ _ _) 'arity3))))))

;(write (pretty-print (expand
;(lambda lst
;  (if-match
;    ()
;    (_)
;    lst
;    'arity1
;    (%pattern-match-lambda
;      ()
;      lst
;      ((_ _) 'arity2)
;      ((_ _ _) 'arity3)))))))

;(write (pretty-print (expand
;((lambda lst
;  ((lambda (alt-thunk17781784)
;     ((lambda ()
;        ;(%husk-switch-to-parent-environment)
;        ((lambda ()
;           ((lambda ()
;              ((lambda () ((lambda () ((lambda () #f))))))))))
;        ((lambda (temp18201827)
;           (if (pair? temp18201827)
;             ((lambda ()
;                ((lambda ()
;                   ((lambda ()
;                      ((lambda ()
;                         ((lambda (_184218611871)
;                            (if (null? (cdr temp18201827))
;                              'arity1
;                              (alt-thunk17781784)))
;                          (car temp18201827))))))))))
;             (alt-thunk17781784)))
;         lst))))
;   (lambda ()
;     (if-match
;       ()
;       (_ _)
;       lst
;       'arity2
;       (%pattern-match-lambda () lst ((_ _ _) 'arity3)))))))
;)))

;((lambda lst
;   ((lambda (alt-thunk177817841777)
;      ((lambda ()
;         ((lambda ()
;            ((lambda ()
;               ((lambda () ((lambda () ((lambda () #f))))))))))
;         ((lambda (temp182018271778)
;            (if (pair? temp182018271778)
;              ((lambda ()
;                 ((lambda ()
;                    ((lambda ()
;                       ((lambda ()
;                          ((lambda (_1842186118711779)
;                             (if (null? (cdr temp182018271778))
;                               'arity1
;                               (alt-thunk177817841777)))
;                           (car temp182018271778))))))))))
;              (alt-thunk177817841777)))
;          lst))))
;    (lambda ()
;      ((lambda (alt-thunk17811787)
;         ((lambda ()
;            ;(%husk-switch-to-parent-environment)
;            (_duplicate-check () () (_ _))
;            (_%if-match
;              ()
;              (_ _)
;              lst
;              'arity2
;              alt-thunk17811787))))
;       (lambda ()
;         (_if-match
;           ()
;           (_ _ _)
;           lst
;           'arity3
;           (_%pattern-match-lambda () lst))))))))

;((lambda () ((lambda () ((lambda () ((lambda () (duplicate-check (_) () (_))))))))))

((lambda () ((lambda () ((lambda () ((lambda () (duplicate-check (_ _) () ())))))))))

;(exam (example3 1 1 1) 'arity3)
;
;;; If underbar was specified as literal, underbar will match literal.
;(define example4
;  (pattern-match-lambda (_)
;    ((_) 'case1)
;    ((x) x)))
;
;(exam (example4 '_) 'case1)
;(exam (example4 'foo) 'foo)
;
;;; If there is duplicate template variables, report error.
;;; (pattern-match-lambda (_)
;;;    ((_) 'case1)
;;;    ((x x) x))
;
;;; If there is fender, use it.
;(define example5
;  (pattern-match-lambda ()
;    ((x) (string? x) x)
;    ((x) 'not-string)))
;
;(exam (example5 "1") "1")
;(exam (example5 1) 'not-string)
