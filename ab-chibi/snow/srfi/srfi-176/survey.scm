(import (scheme base) (scheme write))

(define implementations
  '(("Bigloo"
     ("bigloo"
      ("-v" "(verbose)")
      ("-version")))
    ("Chez Scheme"
     ("scheme"
      ("--version")))
    ("Chibi-Scheme"
     ("chibi-scheme"
      ("-V")))
    ("Chicken"
     ("csc"
      ("-v" "(verbose)")
      ("-version"))
     ("csi"
      ("-version")))
    ("Cyclone"
     ("cyclone"
      ("-v")
      ("-vn" "(version number only)"))
     ("icyc"
      ("-v")
      ("-vn" "(version number only)")))
    ("Dfsch"
     ("dfsch-repl"
      ("-v")))
    ("Foment"
     ("foment"
      ("--version")))
    ("Gambit"
     ("gsc"
      ("-v"))
     ("gsi"
      ("-v")))
    ("Gauche"
     ("gosh"
      ("-V")
      ("-v" "(run another version)")))
    ("Guile"
     ("guile"
      ("-v")
      ("--version")))
    ("Kawa"
     ("kawa"
      ("--version")))
    ("KSi"
     ("ksi"
      ("-v")
      ("-version")
      ("--version")))
    ("Larceny"
     ("larceny"
      ("-version")
      ("--version")))
    ("MIT Scheme"
     ("scheme"
      ("--version")))
    ("Mosh"
     ("mosh"
      ("-V")
      ("-v"))
     ("nmosh"
      ("-V")
      ("-v")))
    ("Oaklisp"
     ("oaklisp"
      ("-v")
      ("-version")
      ("--version")))
    ("Owl Lisp"
     ("ol"
      ("-v")
      ("--version")))
    ("Racket"
     ("racket"
      ("-V")
      ("-v")
      ("--version")))
    ("Sagittarius"
     ("sagittarius"
      ("-v")
      ("-version")
      ("--version"))
     ("sash"
      ("-v")
      ("-version")
      ("--version")))
    ("Schemik"
     ("schemik"
      ("-V")
      ("--version")))
    ("SCM"
     ("scm"
      ("--version")))
    ("Shoe"
     ("shoe"
      ("-v")
      ("--version")))
    ("STklos"
     ("stklos"
      ("-v")
      ("-version")
      ("--version")))
    ("Vicare"
     ("vicare"
      ("-V")
      ("-v" "(verbose)")
      ("--version")
      ("--version-only" "(version number only)")))
    ("Ypsilon"
     ("ypsilon"
      ("--version")))))

(define flags '("-V" "-v" "-version" "--version"))

(define (disp . xs)
  (for-each display xs)
  (newline))

(define (for-head-tail proc xs)
  (let loop ((xs xs))
    (when (pair? xs)
      (let ((x (car xs)))
        (proc (car x) (cdr x))
        (loop (cdr xs))))))

(define (main)
  (disp "<table>")
  (disp "<tr>")
  (disp "<th>Implementation</th>")
  (for-each (lambda (flag) (disp "<td><code>" flag "</code></td>"))
            flags)
  (disp "</tr>")
  (for-head-tail
   (lambda (implementation commands)
     (for-head-tail
      (lambda (command command-flags)
        (disp "<tr>")
        (disp "<td>" implementation " (<code>" command "</code>)</td>")
        (for-each (lambda (flag)
                    (let ((purpose (assoc flag command-flags)))
                      (cond ((not purpose)
                             (disp "<td></td>"))
                            ((null? (cdr purpose))
                             (disp "<td>x</td>"))
                            (else
                             (disp "<td>" (cadr purpose) "</td>")))))
                  flags)
        (disp "</tr>"))
      commands))
   implementations)
  (disp "</table>"))

(main)
