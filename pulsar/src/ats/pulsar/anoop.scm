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
(define an:new-impl (lambda ()
                (cons '() '())))

(define an:new (lambda args
                (let ((constructor (car args))
                      (args (cdr args))
                      )
                  ; (display 'tor )
                  ; (display constructor)
                  ; (newline)
                  ; (display 'args )
                  ; (display args)
                  ; (newline)
                  (apply constructor (append (list (an:new-impl)) args )))))

(define an:r (lambda args
                 (let* ((this   (car  args))
                        (key    (cadr args))
                        (args   (cddr args))
                        (cell   (an:assq key (cdr this)))
                        ;(hello  (begin
                        ;          (display 'this)
                        ;          (display this)
                        ;          (newline)
                        ;          (display 'key)
                        ;          (display key)
                        ;          (newline)
                        ;          (display 'args)
                        ;          (display args)
                        ;          (newline)
                        ;          (display 'cell)
                        ;          (display cell)
                        ;          (newline)
                        ;          (display "(car cell)")
                        ;          (display (car cell))
                        ;          (newline)
                        ;          (display "(cdr cell)")
                        ;          (display (cdr cell))
                        ;          (newline)
                        ;          (display "(cadr cell)")
                        ;          (display (cadr cell))
                        ;          (newline)
                        ;          (display "(cddr cell)")
                        ;          (display (cddr cell))
                        ;          (newline)
                        ;          ))
                        )
                   (if cell 
                     (let ((type (cadr cell)))
                       (cond
                         ((eq? type 'field )
                          (cddr cell))
                         ((eq? type 'method )
                          (apply (cddr cell) (cons this args) )
                          )
                         (else
                           (error 'internal-error))))
                     (error 'not-found)
                     ))))
(define an:i an:r)

(define an:w-impl (lambda (type args)
                      (let* ((this   (car   args))
                             (key    (cadr  args))
                             (value  (caddr args))
                             (args   (cdddr args))
                             (cell   (an:assq key (cdr this))))
                        (if cell
                          (begin
                            (set-cdr! (cdr cell) value )
                            this)
                          (begin
                            (set-cdr! this (alist-cons key (cons type value) (cdr this)) )
                            this)))))

(define an:w (lambda args
                (an:w-impl 'field args)))

(define an:d (lambda args
                (an:w-impl 'method args)))

; (define tst-tor (lambda (this) 
;                   (an:w this 'hello "Hello")
;                   (an:w this 'world "World")
;                   (an:d this 'foo   (lambda (this)
;                                        (display "FOO\n" )))))

; (define tst2 (an:new tst-tor )) 
; (display (an:r tst2 'hello ))(newline)
; (display (an:r tst2 'world ))(newline)
; (display (an:r tst2 'foo   ))(newline)
; (an:w tst2 'world "WORLD")
; (display (an:r tst2 'world ))(newline)

; vim: sw=2 expandtab:
