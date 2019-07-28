(define (hello) (display 'hello-world)(newline))

; (define-library (foo bar bum) hello)
(module-export hello )
(module-name (foo bar bum))

                

;(begin 
;                                (define hello (lambda()
;                                                (display 'hello-world)
;                                                (newline)
;                                                ))
;                                )
;
; vim: expandtab :
