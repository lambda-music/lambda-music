(require "./utest-color.scm" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; utilities
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; examples)
; (utest-execute eq? the-expected-value (a-supposed-to-return-the-expected-value) )
; (utest-execute list? (a-supposed-to-return-list-proc) )
(define utest-execute (lambda (proc . args)
                        (apply proc args)))

(define utest-result 'special-:actual-result:-special )

; Replace utest-result with a specific value
;      (utest-compile-check-command 
;         (list eq? utest-result 'expected-result) 'actual-result )
;      => (list eq? 'actual-result 'expected-result)
(define utest-compile-check-command (lambda (check-command value)
                                      (map 
                                        (lambda (x)
                                          (if (eq? x utest-result )
                                            value
                                            x))
                                        check-command)
                                      ))

; Negate a procedure. ex)
;         (define neq? (utest-negate eq?))
;         (neq? 'hello 'hello)
;             => false
(define utest-negate (lambda (proc)
                            (lambda args
                              (not (apply proc args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; core
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define utest-count 0)
(define utest-error-count 0)


; Basic infrastructure No.1
(define utest-good (lambda (message)
                     (set! utest-count (+ 1 utest-count))
                     (display "  ")
                     (utest-message 'info  
                                    (string-append "[OK] " message ))
                     #t
                     ))
; Basic infrastructure No.2
(define utest-bad (lambda (message)
                    (set! utest-count (+ 1 utest-count))
                    (set! utest-error-count (+ 1 utest-error-count ))
                    (display "  ")
                    (utest-message 'error (string-append "[FAILED] " message ) )
                    #f
                    ))

; Basic assertion.
; ; example
; (utest-assert "test" (list eq? (exec-test) 'expected-result ))
(define utest-assert (lambda (message check-command check-command-description )
                       (display (utest-level-colored 'info "[BEGIN]" ) )
                       (display " ")
                       (display message)
                       (newline)
                       (for-each 
                         (lambda (cmd dsc)
                           (if dsc 
                             (begin
                               ; (set! dsc (string-append "[" dsc "] " ) )
                               (display "  " )
                               (display (utest-level-colored 'info dsc ))
                               (display cmd )
                               (newline))
                             (begin
                               ;do nothing
                               #f)))
                         check-command
                         check-command-description )

                       (if (apply utest-execute check-command )
                         (utest-good message)
                         (utest-bad  message))

                       (display "  " )
                       (newline)
                       ))

; Assertion with exception checking.
;       (utest-assert-proc 
;                          "Check if it really works"
;                          #t ; re-raise the error if test-command raised an error
;                          ; Specify check command
;                          (list eq? utest-result 'expected-result)
;                          ; Specify test command
;                          (list test-proc "arg0" "args1" 'arg2  )
;       )
(define utest-assert-proc (lambda ( message 
                                    re-raise 
                                    check-command 
                                    check-command-description
                                    test-command )
                            (let* ((raised #f)
                                  (actual-value 
                                    (call-with-current-continuation
                                      (lambda (cont)
                                        (with-exception-handler
                                          (lambda (e)
                                            (set! raised e)
                                            (cont e))
                                          (lambda ()
                                            (apply utest-execute test-command)))))))
                              (utest-assert
                                message  
                                (utest-compile-check-command 
                                  check-command
                                  actual-value )
                                check-command-description)
                              (if (and re-raise raised )
                                (raise raised))
                              actual-value
                              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; aliases
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; check command description unary
(define utest-ccd-1  (list #f     "[Result] "  ))
; check command description binary
(define utest-ccd-2  (list #f     "[Expected] " "[Actual]   " ))

(define utest-assert-equality (lambda (message check? expected actual )
                                (utest-assert message 
                                              (list check?  expected  actual   )
                                              (list #f     "[Expected] " "[Actual]   " )
                                              )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; test test
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define utest-test 
  (lambda()
    (utest-assert "TEST-01-OKAY"   (list eq? 'hoo 'hoo ) (list #f "[hoo] " "[hoo] " ) )
    (utest-assert "TEST-01-FAILED" (list eq? 'Foo 'hoo ) (list #f "[hoo] " "[hoo] " ) )

    (define test-1 (lambda (x y)
                     x))


    (utest-assert-proc "TEST-02-OKAY" 
                       #t 
                       (list eq? 'hello         utest-result  )
                       utest-ccd-2
                       (list test-1 'hello 'bar )
                       )


    (utest-assert-proc "TEST-02-FAILED" 
                       #t 
                       (list eq? 'hello         utest-result  )
                       utest-ccd-2
                       (list test-1 'HELLO 'bar )
                       )


    (utest-assert-proc "TEST-03-OKAY" 
                       #t 
                       (list null? utest-result  )
                       utest-ccd-1
                       (list test-1 '() 'bar )
                       )


    (utest-assert-proc "TEST-03-FAILED" 
                       #t 
                       (list null? utest-result  )
                       utest-ccd-1
                       (list test-1 '( hello ) 'bar )
                       )
    (utest-assert-equality "TEST-04-OKAY"   eq? 'hello 'hello  )
    (utest-assert-equality "TEST-04-FAILED" eq? 'hello 'world  )

    ))

(if #f
  (utest-test))

; vim: expandtab shiftwidth=2:
