#!/usr/bin/env emacs --script

(defun slurp-stdin ()
  (condition-case nil (while t (insert (read-from-minibuffer "") "\n"))
    (error)))

(defun read-version-alist ()
  (save-match-data
    (goto-char
     (if (re-search-forward "^(" nil t) (match-beginning 0) (point-max))))
  (let ((xs '()))
    (condition-case nil
        (while t (setq xs (cons (read (current-buffer)) xs)))
      (end-of-file (nreverse xs)))))

(with-temp-buffer
  (slurp-stdin)
  (goto-char (point-min))
  (princ (read-version-alist)))
