(declare (extended-bindings) (not constant-fold) (not safe))

(define f (##not 123))
(define t (##not f))
(define s "")
(define x 1.5)
(define y (##u32vector 111 4294967295))
(define z (##list 1 2 3))

(define (test x)
  (println (##u32vector? x))
  (println (if (##u32vector? x) 11 22)))

(test 0)
(test 1)
(test f)
(test t)
(test s)
(test x)
(test y)
(test z)
(test (##cdr z))
