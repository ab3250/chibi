;;; Package definition file for the Scsh Installation Library
;; Emilio Lopes <eclig@gmx.net>

(define-package "htmlprag"
  (0 13)
  ()

  (install-file "COPYING" 'doc)
  (install-file "README" 'doc)
  
  (install-file '("htmlprag.html" . "index.html")
                'doc "html")

  (install-files '("htmlprag.scm"
                   "packages.scm")
                 'scheme)

  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))
     (user))))
