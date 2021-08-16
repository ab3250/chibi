;; please see copyright notice in ./COPYING

(define-library (srfi 198 test)
  (export run-tests)

  (import (scheme base)
          (chibi)
          ;; (only (chibi process) exit)
          (chibi test)

          (srfi 198)
          )

  (begin

    ;; Inverse of test-error, mutated from test-not, only errors if an
    ;; exception is raised

    (define-syntax test-not-error
      (syntax-rules ()
        ((_ expr) (test-assert (begin expr #t)))
        ((_ name expr) (test-assert name (begin expr #t)))))

    (define the-error #f)

    (define (run-tests)

      (test-group "srfi-198: POSIX system call exceptions"

#|
        (test-group "Prologue"

          (test 1 1)

          ) ;; end prologue
|#

        (test-group "Main tests"

          (set! the-error (make-foreign-error 1))
          (test 'error (foreign-error:error-set the-error))
          (test #f (foreign-error:code the-error))
          (test 'make-foreign-error (foreign-error:scheme-procedure the-error))
          (test #f (foreign-error:foreign-interface the-error))
          (test "Malformed call to make-foreign-error, not a list; see data for details" (foreign-error:message the-error))
          (test '((arguments . 1)) (foreign-error:data the-error))

          (set! the-error (make-foreign-error '(1)))
          (test 'error (foreign-error:error-set the-error))
          (test #f (foreign-error:code the-error))
          (test 'make-foreign-error (foreign-error:scheme-procedure the-error))
          (test #f (foreign-error:foreign-interface the-error))
          (test "Malformed call to make-foreign-error, not an alist; see data for details" (foreign-error:message the-error))
          (test '((arguments 1)) (foreign-error:data the-error))

          (set! the-error (make-foreign-error '((1 . 1))))
          (test 'error (foreign-error:error-set the-error))
          (test #f (foreign-error:code the-error))
          (test 'make-foreign-error (foreign-error:scheme-procedure the-error))
          (test #f (foreign-error:foreign-interface the-error))
          (test "Malformed call to make-foreign-error, first key must be a symbol; see data for details" (foreign-error:message the-error))
          (test '((arguments (1 . 1))) (foreign-error:data the-error))

          (set! the-error (make-foreign-error '((foo . 1))))
          (test 'error (foreign-error:error-set the-error))
          (test #f (foreign-error:code the-error))
          (test 'make-foreign-error (foreign-error:scheme-procedure the-error))
          (test #f (foreign-error:foreign-interface the-error))
          (test "Malformed call to make-foreign-error, missing error-set; see data for details" (foreign-error:message the-error))
          (test '((arguments (foo . 1))) (foreign-error:data the-error))

          (set! the-error (make-foreign-error '((error-set . error))))
          (test 'error (foreign-error:error-set the-error))
          (test #f (foreign-error:code the-error))
          (test #f (foreign-error:scheme-procedure the-error))
          (test #f (foreign-error:foreign-interface the-error))
          (test #f (foreign-error:message the-error))
          (test #f (foreign-error:data the-error))

          ;; Make sure the error raising code isn't malfunctioning and raising a different error
          (test-error ((with-exception-handler
                        (lambda (exception) (set! the-error exception))
                        (lambda () (raise-foreign-error '((error-set . error)))))))
          (test-assert (foreign-error? the-error))

          ;; Make a "real" error
          (set! the-error (make-foreign-error '((error-set . errno)
                                                (code . ((number . 2)
                                                         (symbol . errno/ENOENT)))
                                                (scheme-procedure . open-file)
                                                (foreign-interface . open)
                                                (message . "open-file called open: errno/ENOENT: No such file or directory")
                                                (data . ((arguments . ("not-a-valid-filename" 0 428))
                                                         (heritage . "SRFI 170"))))))

          (test 'errno (foreign-error:error-set the-error))
          (test 2 (cdr (assq 'number (foreign-error:code the-error))))
          (test 'errno/ENOENT (cdr (assq 'symbol (foreign-error:code the-error))))
          (test 'open-file (foreign-error:scheme-procedure the-error))
          (test 'open (foreign-error:foreign-interface the-error))
          (test "open-file called open: errno/ENOENT: No such file or directory" (foreign-error:message the-error))
          (test '("not-a-valid-filename" 0 428) (cdr (assq 'arguments (foreign-error:data the-error))))
          (test "SRFI 170" (cdr (assq 'heritage (foreign-error:data the-error))))


          ) ;; end errors

#|
        (test-group "Epilogue"

          (test 1 1)

          ) ;; end epilogue
|#

        ))))
