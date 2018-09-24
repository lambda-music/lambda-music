; =========================================================
; XNOOP (XNOOP is not an OOP Library)
; This file is distributed with the copyright which is described at GPL3
; =========================================================

(import (srfi 1))

(define xassq (lambda (key lst)
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
(define xn-impl (lambda ()
                (cons '() '())))

(define xn (lambda args
                (let ((constructor (car args))
                      (args (cdr args))
                      )
                  ; (display 'tor )
                  ; (display constructor)
                  ; (newline)
                  ; (display 'args )
                  ; (display args)
                  ; (newline)
                  (apply constructor (append (list (xn-impl)) args )))))

(define xd (lambda (modifier args)
               (let* ((modifier   modifier)
                      (this   (car   args))
                      (key    (cadr  args))
                      (value  (caddr args))
                      (args   (cdddr args))
                      (fields (cdr   this))
                      (cell   (xassq key fields)))
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

                     (let ((type     (cdr (or (assq 'type modifier)
                                              (cons 'type 'unknown )))))
                       (cond
                         ((eq? type 'field )
                          (letrec* ((field-key (string->symbol 
                                                 (string-append 
                                                   "*FIELD*-" 
                                                   (symbol->string key))))
                                    (field-value value)
                                    (field-accessor (lambda args
                                                      (if (= 0 (length args))
                                                        ; Do nothing. This will never be happen.
                                                        #f
                                                        ;write
                                                        (let ((this   (car args))
                                                              (args   (cdr args))
                                                              (fields (cdr this))
                                                              (cell   (or (xassq field-key fields)
                                                                        (cons key #f))))
                                                          (if (= 0 (length args))
                                                            ;read
                                                            (begin
                                                              (cdr cell))
                                                            ;write
                                                            (let ((new-value (car args)))
                                                              (set-cdr! cell new-value)
                                                              new-value)
                                                            )))
                                                      ))
                                    
                                    )
                                   (set! fields (alist-cons field-key field-value    fields))
                                   (set! fields (alist-cons key       field-accessor fields))
                                   (set-cdr! this fields)
                                   ))
                         ((eq? type 'method )
                          (set-cdr! this (alist-cons key value fields)))
                         (else
                           (error "an internal error occured "))))

                     
                     this)))))


(define xx (lambda args
                 (let* ((this   (car  args))
                        (key    (cadr args))
                        (args   (cddr args))
                        (cell   (xassq key (cdr this))))

                   ; (display 'cell-type)
                   ; (display (assq 'type (cadr cell)))
                   ; (newline)
 
                   (if cell
                     (let ((field-accessor (cdr cell)))
                       (apply field-accessor (cons this args)))
                     (error (string-append "referred a not defined field (" key ")" ) )
                     ))))

(define xxi xx)
(define xxw xx)
(define xxr xx)
(define xdf (lambda args
                (xd 'field args)))

(define xdm (lambda args
                (xd 'method args)))



; (define tst-tor (lambda (this) 
;                   (xxw this 'hello "Hello")
;                   (xxw this 'world "World")
;                   (xdm this 'foo   (lambda (this)
;                                        (display "FOO\n" )))))

; (define tst2 (xn tst-tor )) 
; (display (xx tst2 'hello ))(newline)
; (display (xx tst2 'world ))(newline)
; (display (xx tst2 'foo   ))(newline)
; (xxw tst2 'world "WORLD")
; (display (xx tst2 'world ))(newline)

; vim: sw=2 expandtab:
