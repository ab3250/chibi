(import (rnrs))

(define (read-version-alist in)
  (let skip-until-paren ((line-start? #t))
    (let ((c (peek-char in)))
      (if (not (or (eof-object? c) (and line-start? (char=? c #\())))
          (skip-until-paren (eqv? #\newline (read-char in)))
          (let read-all ((xs '()))
            (let ((x (read in)))
              (if (eof-object? x) (reverse xs) (read-all (cons x xs)))))))))

(for-each (lambda (x) (write x) (newline))
          (read-version-alist (current-input-port)))
