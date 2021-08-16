(define-library (delay-chibi)
(import
    (scheme base)
    (scheme red)
    (websocket)
    (chibi time))

(define (delay sec)
    (define start (current-seconds))
    (let timeloop ()    
        (if ( < (- (current-seconds) start) sec) (timeloop)))))

