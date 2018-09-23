; =========================================================
; Anoop (Anoop is not an OOP Library)
; This file is distributed with the copyright which is described at GPL3
; =========================================================

(import (srfi 1))

(define an:assq (lambda (key lst)
                    (let loop ((lst lst))
                      (if (null? lst)
                        #f
                        (let ((curr (car lst)))
                          (if (eq? key (car curr))
                            curr
                            (loop (cdr lst))))))))

; (define-syntax an-pop
;   (syntax-rules ()
;                 ((an-pop variable )
;                  (if (null? variable )
;                    (error 'argument-underflow-error )
;                    (let ((value (car variable)))
;                      (set! variable (cdr variable))
;                      value)))))

; create
(define an:n-impl (lambda ()
                (cons '() '())))

(define hello-hello 'HELLO)

(define an:n (lambda args
                (let ((constructor (car args))
                      (args (cdr args))
                      )
                  ; (display 'tor )
                  ; (display constructor)
                  ; (newline)
                  ; (display 'args )
                  ; (display args)
                  ; (newline)
                  (apply constructor (append (list (an:n-impl)) args )))))

(define an:r (lambda args
                 (let* ((this   (car  args))
                        (key    (cadr args))
                        (args   (cddr args))
                        (cell   (an:assq key (cdr this))))

                   ; (display 'cell-type)
                   ; (display (assq 'type (cadr cell)))
                   ; (newline)
 
                   (if cell
                     (let* ((modifier (cadr cell))
                            (type     (cdr (or (assq 'type modifier)
                                               (cons 'type 'unknown ))))
                            (value    (cddr cell)))
                       (cond
                         ((eq? type 'field )
                          value)
                         ((eq? type 'method )
                          (apply value (cons this args) ))
                         (else
                           (error "an internal error occured "))))
                     (error (string-append "referred a not defined field (" key ")" ) )
                     ))))
(define an:i an:r)

(define an:d (lambda (modifier args)
               (let* ((modifier   modifier)
                      (this   (car   args))
                      (key    (cadr  args))
                      (value  (caddr args))
                      (args   (cdddr args))
                      (cell   (an:assq key (cdr this))))
                 (if cell
                   (error (string-append "atempted to define an already defined variable name (" 
                                         (symbol->string key ) ")" ) )

                   (begin
                     (set! modifier (if (not (list? modifier))
                                      (if (pair? modifier)
                                        (list modifier )
                                        (list (cons 'type modifier )))
                                      modifier))

                     (set! modifier (map (lambda (elem)
                                           (if (not (pair? elem))
                                             (cons elem '() )
                                             elem)
                                           ) modifier))

                     (set-cdr! this (alist-cons key (cons modifier value) (cdr this)) )
                     this)))))

(define an:w (lambda args
               (let* ((this   (car   args))
                      (key    (cadr  args))
                      (value  (caddr args))
                      (args   (cdddr args))
                      (cell   (an:assq key (cdr this))))
                 (if cell
                   (begin
                     ;(display 'cell-w )
                     ;(display (cadr cell))
                     ;(newline)
                     (if (assq 'read-only (cadr cell))
                       (error "atempted to update a read-only field."))
                     (set-cdr! (cdr cell) value )
                     this)
                   (begin
                     (error (string-append "referred a not defined field (" key ")" ) ))))))

(define an:df (lambda args
                (an:d 'field args)))

(define an:dm (lambda args
                (an:d 'method args)))

; (define tst-tor (lambda (this) 
;                   (an:w this 'hello "Hello")
;                   (an:w this 'world "World")
;                   (an:dm this 'foo   (lambda (this)
;                                        (display "FOO\n" )))))

; (define tst2 (an:n tst-tor )) 
; (display (an:r tst2 'hello ))(newline)
; (display (an:r tst2 'world ))(newline)
; (display (an:r tst2 'foo   ))(newline)
; (an:w tst2 'world "WORLD")
; (display (an:r tst2 'world ))(newline)

; vim: sw=2 expandtab:
