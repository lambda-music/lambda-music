; =========================================================
; Anoop (Anoop is not an OOP Library)
; This file is distributed with the copyright which is described at GPL3
; =========================================================

(import (srfi 1))

(define a*assq (lambda (key lst)
                    (let loop ((lst lst))
                      (if (null? lst)
                        #f
                        (let ((curr (car lst)))
                          (if (eq? key (car curr))
                            curr
                            (loop (cdr lst))))))))

(define-syntax ++
  (syntax-rules ()
                ((++ var )
                 (if (null? var )
                   (error 'argument-underflow-error )
                   (let ((value (car var)))
                     (set! var (cdr var))
                     value)))))

(define a*ate (lambda ()
                (cons '() '())))

(define a*new (lambda args
                (let ((tor (++ args)))
                  (apply tor (append (list (a*ate)) args )))))

(define a*get (lambda args
                 (let* ((args   args)
                        (this   (++ args))
                        (key    (++ args))
                        (cell   (a*assq key (cdr this))))
                   (and cell (let ((type (cadr cell)))
                               (cond
                                 ((eq? type 'field )
                                  (cddr cell))
                                 ((eq? type 'method )
                                  (apply (cddr cell) (cons cell args) )
                                  )
                                 (else
                                   (error 'internal-error))))))))

(define a*set-impl (lambda (type args)
                 (let* ((args   args)
                        (this   (++ args))
                        (key    (++ args))
                        (value  (++ args))
                        (cell   (a*assq key (cdr this))))
                   (if cell
                     (begin
                       (set-cdr! (cdr cell) value )
                       this)
                     (begin
                       (set-cdr! this (alist-cons key (cons type value) (cdr this)) )
                       this)))))

(define a*set (lambda args
                (a*set-impl 'field args)))

(define a*def (lambda args
                (a*set-impl 'method args)))

; (define tst-tor (lambda (this) 
;                   (a*set this 'hello "Hello")
;                   (a*set this 'world "World")
;                   (a*def this 'foo   (lambda (this)
;                                        (display "FOO\n" )))))

; (define tst2 (a*new tst-tor )) 
; (display (a*get tst2 'hello ))(newline)
; (display (a*get tst2 'world ))(newline)
; (display (a*get tst2 'foo   ))(newline)
; (a*set tst2 'world "WORLD")
; (display (a*get tst2 'world ))(newline)

; vim: sw=2 expandtab:
