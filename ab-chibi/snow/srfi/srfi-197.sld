(define-library (srfi srfi-197 srfi-197)
  (export -> ->> as->
          some-> some->>
          cond-> cond->>
          lambda-> lambda->>)

  (import (scheme base))

  (include "srfi-197.scm"))
