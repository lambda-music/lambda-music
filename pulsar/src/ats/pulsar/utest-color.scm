;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; color
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define utest-color-table
  `(( black      . "\x1b[0;30m" ) ( dark-gray    . "\x1b[1;30m" )
    ( red        . "\x1b[0;31m" ) ( light-red    . "\x1b[1;31m" )
    ( green      . "\x1b[0;32m" ) ( light-green  . "\x1b[1;32m" )
    ( orange     . "\x1b[0;33m" ) ( yellow       . "\x1b[1;33m" )
    ( blue       . "\x1b[0;34m" ) ( light-blue   . "\x1b[1;34m" )
    ( purple     . "\x1b[0;35m" ) ( light-purple . "\x1b[1;35m" )
    ( cyan       . "\x1b[0;36m" ) ( light-cyan   . "\x1b[1;36m" )
    ( light-gray . "\x1b[0;37m" ) ( white        . "\x1b[1;37m" )
    ( reset      . "\x1b[0m" )))

(define utest-color (lambda ( color )
                      (cdr (or (assq color utest-color-table) 
                               '( reset ."\x1b[0m" )))))

(define utest-colored (lambda ( color msg )
                        (string-append (utest-color color)
                                       msg
                                       (utest-color 'reset ))))

(define utest-level-table 
  `((info     . ,(utest-color 'light-blue   ))
    (warn     . ,(utest-color 'yellow       ))
    (error    . ,(utest-color 'light-red    ))
    (critical . ,(utest-color 'light-purple ))
    (reset    . ,(utest-color 'reset        ))))

(define utest-level-color (lambda ( level )
                            (cdr (or (assq level utest-level-table ) 
                                     '( reset ."\x1b[0m" )))))

(define utest-level-colored (lambda ( level msg )
                              (string-append (utest-level-color level )
                                             msg
                                             (utest-level-color 'reset ))))

(define utest-message (lambda (level msg)
                        (display 
                          (string-append (utest-level-color level)
                                         msg
                                         (utest-level-color 'reset)
                                         ))
                        (newline)))


(define utest-color-test 
  (lambda()
    (utest-message 'info       "HELLO")
    (utest-message 'warn       "HELLO")
    (utest-message 'error      "HELLO")
    (utest-message 'critical   "HELLO")
    ))

(if #f
  (utest-color-test))


; vim: expandtab shiftwidth=2:
