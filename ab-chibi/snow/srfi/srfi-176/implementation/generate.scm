(import (scheme base) (scheme char) (scheme file) (scheme write))

(define (append-map f xs)
  (let loop ((xs xs) (acc '()))
    (if (null? xs) acc (loop (cdr xs) (append acc (f (car xs)))))))

(define (disp . xs)
  (for-each display xs)
  (newline))

(define (call-with-string-io s thunk)
  (call-with-port
   (open-input-string s)
   (lambda (in)
     (call-with-port (open-output-string)
                     (lambda (out)
                       (parameterize ((current-input-port in)
                                      (current-output-port out))
                         (thunk))
                       (get-output-string out))))))

;;

(define (dashes->underscores s)
  (call-with-string-io
   s (lambda ()
       (let loop ()
         (let ((char (read-char)))
           (unless (eof-object? char)
             (write-char (case char
                           ((#\- #\.) #\_)
                           (else char)))
             (loop)))))))

(define (dashes->capitalized s)
  (call-with-string-io
   s (lambda ()
       (let loop ((cap? #t))
         (let ((char (read-char)))
           (unless (eof-object? char)
             (case char
               ((#\- #\.)
                (loop #t))
               (else
                (write-char (if cap? (char-upcase char) char))
                (loop #f)))))))))

(define (regexp-quote s)
  (call-with-string-io
   s (lambda ()
       (let loop ()
         (let ((char (read-char)))
           (unless (eof-object? char)
             (when (char=? #\. char)
               (write-char #\\))
             (write-char char)
             (loop)))))))

;;

(define table
  '((command             string)
    (scheme.id           symbol)
    (languages           symbol-list)
    (scheme.features     symbol-list)
    (c.type-bits         subsymbols int long float double pointer size_t)
    (c.version           string)
    (c.compile           string-list)
    (c.link              string-list)
    (build.configure     string-list)

    (build.git.tag       string)
    (build.git.branch    string)
    (build.git.commit    string)
    (build.git.modified  string-list)

    (build.date          string)
    (image.date          string)
    (image.file          string)
    (install-dir         string)
    (scheme.path         string-list)
    (release             string)
    (release.date        string)
    (release.name        string)
    (website             string)))

(define (entry-name entry) (symbol->string (car entry)))
(define (entry-type entry) (cadr entry))
(define (entry-subs entry) (map symbol->string (cddr entry)))

;;

(define (write-file filename thunk)
  (disp "Writing " filename)
  (with-output-to-file filename thunk))

(define (write-unix-shell-script filename entry->pipeline)
  (write-file
   filename
   (lambda ()
     (disp "#!/bin/sh")
     (disp "# Auto-generated")
     (disp "set -eu")
     (disp "alias awk=\"${AWK:-awk}\"")
     (disp "alias grep=\"${GREP:-grep}\"")
     (disp "alias sed=\"${SED:-sed}\"")
     (disp "input=\"$(cat)\"")
     (let ((alist (append-map entry->pipeline table)))
       (for-each (lambda (entry)
                   (disp (dashes->underscores (car entry))
                         "=$(echo \"$input\" | " (cdr entry) ")"))
                 alist)
       (for-each (lambda (entry)
                   (disp "echo \"" (car entry) ": "
                         "$" (dashes->underscores (car entry)) "\""))
                 alist)))))

(define (write-powershell-script filename entry->pipeline)
  (write-file
   filename
   (lambda ()
     (disp "#!/usr/bin/env pwsh")
     (disp "# Auto-generated")
     (disp "$InputArray = @($Input)")
     (let ((alist (append-map entry->pipeline table)))
       (for-each (lambda (entry)
                   (disp "$" (dashes->capitalized (car entry))
                         " = ($InputArray" (cdr entry) ")"))
                 alist)
       (for-each (lambda (entry)
                   (disp "Write-Host \"" (car entry) ": "
                         "$" (dashes->capitalized (car entry)) "\""))
                 alist)))))

;;

(define (awk/string entry)
  (let ((name (entry-name entry)))
    (list (cons name
                (string-append
                 "awk"
                 " '/^\\(" (regexp-quote name) " .*\\)$/"
                 " {sub(/^\\([a-z.-]+ \"/, \"\", $0);"
                 " sub(/\".*/, \"\", $0);"
                 " print $0}'")))))

(define (awk/symbol entry)
  (let ((name (entry-name entry)))
    (list (cons name
                (string-append
                 "awk"
                 " '/^\\(" (regexp-quote name) " .*\\)$/"
                 " {sub(/^\\([a-z.-]+ /, \"\", $0);"
                 " sub(/\\)$/, \"\", $0);"
                 " print $0}'")))))

(define (awk/subsymbols entry)
  (let ((top (entry-name entry)))
    (map (lambda (sub)
           (cons (string-append top "-" sub)
                 (string-append
                  "awk"
                  " '/^\\(" (regexp-quote top)
                  " .*\\(" (regexp-quote sub) " .*\\).*\\)$/"
                  " {sub(/.*\\(" (regexp-quote sub) " /, \"\", $0);"
                  " sub(/\\).*/, \"\", $0);"
                  " print $0}'")))
         (entry-subs entry))))

;;

(define (grep-sed/string entry)
  (let ((name (entry-name entry)))
    (list (cons name
                (string-append
                 "grep '^(" (regexp-quote name) " \".*\".*)$'"
                 " | "
                 "sed"
                 " -e 's@^([a-z.-]* \"@@'"
                 " -e 's@\".*@@'")))))

(define (grep-sed/symbol entry)
  (let ((name (entry-name entry)))
    (list (cons name
                (string-append
                 "grep '^(" (regexp-quote name) " .*)$'"
                 " | "
                 "sed"
                 " -e 's@^([a-z.-]* @@'"
                 " -e 's@)$@@'")))))

(define (grep-sed/subsymbols entry)
  (let ((top (entry-name entry)))
    (map (lambda (sub)
           (cons (string-append top "-" sub)
                 (string-append
                  "grep '^(" (regexp-quote top)
                  " .*(" (regexp-quote sub) " .*).*)$'"
                  " | "
                  "sed"
                  " -e 's@^.*(" (regexp-quote sub) " @@'"
                  " -e 's@).*@@'")))
         (entry-subs entry))))

;;

(define (powershell/string entry)
  (let ((name (entry-name entry)))
    (list (cons name
                (string-append
                 " | Select-String -Pattern"
                 " '^\\(" (regexp-quote name) " \"([^\"]*)\"\\)$'"
                 " | ForEach-Object {$_.Matches[0].Groups[1].Value}")))))

(define (powershell/symbol entry)
  (let ((name (entry-name entry)))
    (list (cons name
                (string-append
                 " | Select-String -Pattern"
                 " '^\\(" (regexp-quote name) " (.*)\\)$'"
                 " | ForEach-Object {$_.Matches[0].Groups[1].Value}")))))

(define (powershell/subsymbols entry)
  (let ((top (entry-name entry)))
    (map (lambda (sub)
           (cons (string-append top "-" sub)
                 (string-append
                  " | Select-String -Pattern"
                  " '^\\(" (regexp-quote top)
                  " .*\\(" (regexp-quote sub) " (.*?)\\).*\\)$'"
                  " | ForEach-Object {$_.Matches[0].Groups[1].Value}")))
         (entry-subs entry))))

;;

(define (awk)
  (write-unix-shell-script
   "read-using-awk.sh"
   (lambda (entry)
     ((case (entry-type entry)
        ((string) awk/string)
        ((symbol symbol-list string-list) awk/symbol)
        ((subsymbols) awk/subsymbols)
        (else (error "N/A")))
      entry))))

(define (grep-sed)
  (write-unix-shell-script
   "read-using-grep-sed.sh"
   (lambda (entry)
     ((case (entry-type entry)
        ((string) grep-sed/string)
        ((symbol symbol-list string-list) grep-sed/symbol)
        ((subsymbols) grep-sed/subsymbols)
        (else (error "N/A")))
      entry))))

(define (powershell)
  (write-powershell-script
   "read-using-powershell.ps1"
   (lambda (entry)
     ((case (entry-type entry)
        ((string) powershell/string)
        ((symbol symbol-list string-list) powershell/symbol)
        ((subsymbols) powershell/subsymbols)
        (else (error "N/A")))
      entry))))

(define (c-stdio)
  (write-file
   "read-using-c.c"
   (lambda ()
     (disp "/* Auto-generated */")
     (disp "#include <stdio.h>")
     (disp)
     (disp "#include \"read-using-c-helpers.h\"")
     (disp)
     (disp "int main(void) {")
     (disp "  init_from_stream(stdin);")
     (for-each (lambda (entry)
                 (let ((top (entry-name entry)))
                   (case (entry-type entry)
                     ((string)
                      (disp "  printf(\"" top
                            ": %s\\n\", parse_string(\"" top "\"));"))
                     ((symbol symbol-list string-list)
                      (disp "  printf(\"" top
                            ": %s\\n\", parse_symbol(\"" top "\"));"))
                     ((subsymbols)
                      (for-each (lambda (sub)
                                  (disp "  printf(\"" top "-" sub
                                        ": %s\\n\", parse_subsymbol(\"" top
                                        "\", \"" sub "\"));"))
                                (entry-subs entry)))
                     (else (error "N/A")))))
               table)
     (disp "  return 0;")
     (disp "}"))))

(awk)
(grep-sed)
(powershell)
(c-stdio)
