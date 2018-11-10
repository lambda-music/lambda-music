; 
; Pulsar-Sequencer written by Atsushi Oka 
; Copyright 2018 Atsushi Oka
; 
; This file is part of Pulsar-Sequencer. 
; 
; Pulsar-Sequencer is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; Pulsar-Sequencer is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
; 


; =========================================================
; XNOOP (XNOOP is not an OOP Library)
; =========================================================

(import (srfi 1))
(import (kawa pprint))

(define xnoop-list? (lambda (x)
                      (and (list? x)
                           (eq? (car x) 'XNOOP-LIST))
                      ))
(define xnoop-data? (lambda(x)
                      (and (pair? x) 
                           (xnoop-list? (car x))
                           (xnoop-list? (cdr x)))
                      ))
(define xnoop? (lambda (x)
                 (and (procedure? x) 
                      (eq? (x 'xnoop?) 'is-xnoop ))))

(define xacons (lambda (field-key field-value    fields)
                 ; (reverse
                 ;   (alist-cons field-key field-value (reverse fields) ))
                 (append
                   fields 
                   (list (cons field-key field-value)))))

(define xassq (lambda (key lst)
                ; (display 'xassq)
                ; (display (tostr lst 1))
                ; (newline)

                ; (if (not (xnoop-list? lst))
                ;   (raise (cons 'xnoop-bad-list-error "invalid xnoop list error")))

                ; The reason (cdr lst)
                ; Skip the first element because it should be the constant
                ; identifier 'XNOOP-LIST
                (let loop ((lst (cdr lst)))
                  (if (null? lst)
                    #f
                    (let ((curr (car lst)))
                      (if (eq? key (car curr))
                        curr
                        (loop (cdr lst))))))))


(define xdefine (lambda args
             (let* ((this      (car    args))
                    (modifier  (cadr   args))
                    (key       (caddr  args))
                    (value     (cadddr args))
                    (args      (cddddr args))
                    (fields    (cdr   this))
                    (cell      (xassq key fields)))
               (if cell
                 (raise 
                   (cons 'duplicate-field-error
                         (string-append "atempted to define an already " 
                                        "defined variable name (" 
                                        (symbol->string key ) ")" )))

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
                                 (set! fields (xacons field-key field-value    fields))
                                 (set! fields (xacons key       field-accessor fields))
                                 (set-cdr! this fields)
                                 ))
                       ((eq? type 'method )
                        (set-cdr! this (xacons key value fields)))
                       (else
                         (raise (cons 'internal-error "an internal error occured ")))))


                   this)))))


(define xdummy (lambda (self . args)
                 (apply self args)))

; create
(define xnew-instance (lambda ()
                        (letrec (( this (cons 
                                          ; A symbol which contains its class name. TODO this is obsolete.
                                          (list
                                            'XNOOP-LIST
                                            )
                                          ; An alist which contains its field cells.
                                          (list
                                            ; the constant identifier
                                            'XNOOP-LIST
                                            ; 'this field is the foundation of this system.
                                            (cons
                                              'xnoop? (lambda (self)
                                                        (if (xnoop-data? this)
                                                          'is-xnoop
                                                          #f)))
                                            (cons
                                              'this   (lambda args this ))
                                            (cons
                                              'put    (lambda (self key value)
                                                        (set-cdr! this (xacons key value (cdr this)))))
                                            (cons
                                              'get    (lambda (self key)
                                                        (let ((cell (xassq key (cdr this))))
                                                          (if cell
                                                            (cdr cell)
                                                            #f))))
                                            (cons
                                              'define (lambda (self . args)
                                                        (apply xdefine (cons this args ))))

                                            (cons 'invoke xdummy)
                                            (cons 'read   xdummy)
                                            (cons 'write  xdummy)
                                            )))
                                 (self (lambda (key . args)
                                         (let ((cell (xassq key (cdr this))))
                                           (if cell
                                             (let ((method (cdr cell)))
                                               (apply method (cons self args)))
                                             (raise 
                                               (cons
                                                 'not-found-error
                                                 (string-append "referred a not defined field (" 
                                                                (symbol->string key) 
                                                                ")" ))))))

                                       ))
                          self)))


(define xnew (lambda (constructor . args)
             (let ((self (xnew-instance)))
               (apply constructor  (cons self args))
               self)))



(define tostr (lambda (x indent ) 
                (cond
                  ((string? x)
                   (string-append "\""                 x  "\"" )
                   )
                  ((number? x)
                   (number->string x)
                   )
                  ((symbol? x)
                   (string-append "'"  (symbol->string x)      )
                   )
                  ((null? x)
                   "'()"
                   )
                  ((list? x)
                   (string-append 
                     "(list "
                     (let loop ((y x)(str ""))
                       (if (null? y)
                         str
                         ;(string-append str "\n'()" )
                         (loop (cdr y) (string-append str "\n" (tostr (car y) indent )))
                         ))
                     ")"
                     ))
                  ((pair? x)
                   (string-append "(cons "  
                                  (tostr (car x) (+ indent 1))
                                  " "
                                  (tostr (cdr x) (+ indent 1))
                                  ")"))
                  ((procedure? x)
                   (string-append "(lambda (...) "  
                                  (let ((val (procedure-property x 'name) ))
                                    (if val val "noname"))
                                  ")"))
                  (else
                    (raise (cons 'unsupported-type "unknown type" )))
                    )
                  )
                )


; vim: sw=2 expandtab:
