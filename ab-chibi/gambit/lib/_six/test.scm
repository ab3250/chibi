;;;============================================================================

;;; File: "test.scm"

;;; Copyright (c) 2020-2021 by Marc Feeley All Rights Reserved.

;;;============================================================================

(import _six/js)
(import _test)

;; TODO: add more!

(test-equal
  "(lambda ()\n  (##host-define-function-and-call-dynamic\n   '#&\"g_function2scm(async function () {\\n return 0;\\n})\"))\n"
  (call-with-output-string (lambda (port) (pp (lambda () \0) port))))

;;;============================================================================
