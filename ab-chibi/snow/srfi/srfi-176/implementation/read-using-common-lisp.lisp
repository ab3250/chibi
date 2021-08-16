#!/usr/bin/env clisp

(defun read-version-alist (in)
  (let ((line-start? t))
    (loop (let ((c (peek-char nil in nil)))
            (when (or (not c) (and line-start? (char= c #\())) (return))
            (setf line-start? (char= #\newline (read-char in))))))
  (with-standard-io-syntax
    (let ((xs '()) (eof (gensym)) (*read-eval* nil))
      (loop (let ((x (read in nil eof)))
              (when (eql eof x) (return (nreverse xs)))
              (push x xs))))))

(write (read-version-alist *standard-input*) :pretty t :case :downcase)
