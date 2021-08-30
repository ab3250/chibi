(define-interface htmlprag-interface
  (export shtml-comment-symbol
          shtml-decl-symbol
          shtml-empty-symbol
          shtml-end-symbol
          shtml-entity-symbol
          shtml-pi-symbol
          shtml-start-symbol
          shtml-text-symbol
          shtml-top-symbol

          shtml-named-char-id
          shtml-numeric-char-id

          make-shtml-entity
          make-shtml-entity

          make-html-tokenizer
          tokenize-html
          shtml-token-kind

          parse-html/tokenizer

          html->sxml-0nf
          html->sxml-1nf
          html->sxml-2nf
          html->sxml
          html->shtml

          write-shtml-as-html
          shtml->html))

(define-structure htmlprag htmlprag-interface
  (open scheme
        ascii
        srfi-6
        srfi-23)
  (begin
    (define integer->char ascii->char))
  (files htmlprag))
