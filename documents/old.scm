(define-macro use-srfi (lambda (name)
  `(import (srfi ,name))))

(use-srfi 1)
(use-srfi 13)
(use-srfi 14)

(define text->list:not-newline (char-set-complement (char-set #\newline )))
(define text->list:tab-size 8)

(define (text->list s)
  (let* ((phase-1 
           (let loop-1 ((s1 (string-tokenize s text->list:not-newline )))
             ; parsed-text
             (if (null? s1)
               '()
               (if (= 0 (string-length (string-trim-both (car s1)) ))
                 ;skip
                 (loop-1 (cdr s1))
                 (cons 
                   (let-values ((( is-header indent-level text ) 
                                 (let loop-2 ((s2 (string->list (car s1)))
                                              (count 0))
                                   (cond 
                                     ((null? s2)
                                      (values #f count "" ))
                                     ((eqv? (car s2) #\tab)
                                      (loop-2 (cdr s2 ) (+ count text->list:tab-size)))
                                     ((eqv? (car s2)  #\space)
                                      (loop-2 (cdr s2 ) (+ count 1                  )))
                                     ((eqv? (car s2)  #\=)
                                      (values #t count (list->string (cdr s2) ))
                                      )
                                     (else
                                       (values #f count (list->string s2 ) ))))))
                               (if (= 0 (string-length text ))
                                 #f
                                 (list 
                                   (cons 'text (string-trim-both text ) )
                                   (cons 'indent-level indent-level)
                                   (cons 'is-header is-header))
                                 ))
                   (loop-1 (cdr s1)))))))

         (phase-2 
           (let* ((parsed-text phase-1)
                  (state:top-element (list 'top ))
                  (state:stack       (list 'stack ))
                  (state:current-level -1 ))

             (display parsed-text)
             (newline)
             ; push the root element to the stack
             (set-cdr! state:stack (cons state:top-element (cdr state:stack)))

             ; phase-4
             (let loop-4 ((s4 parsed-text))
               (newline)
               ; (display (format "(length state:stack) ~a\n" (length state:stack)))
               (display (format "state:current-level ~a\n" state:current-level))
               (display (format "(length (cadr state:stack) ) ~a\n" (length (cadr state:stack) )))
               (display (format "(length state:top-element ) ~a\n" (length state:top-element )))

               (if (null? s4)
                 (begin
                   (display "end\n")
                   #f)
                 (if (not (car s4) )
                   ; if car s4 is #f, then ignore it.
                   (loop-4 (cdr s4))
                   (let ((current-level
                           (cdr
                             (or (assq 'indent-level (car s4 ))
                                 (cons 'indent-level 0 )))))
                     ; this is done when it is the first time only.
                     (if (< state:current-level 0 )
                       (begin
                         (set! state:current-level current-level)
                         (display "initialized\n" )
                         ))

                     (cond
                       ((<                 state:current-level current-level )
                        (display "descend\n")
                        ; push the last header element of the header elmenent on the stack
                        (let ((the-last-element (cadadr state:stack)))
                          (if (pair? the-last-element )
                            (set-cdr! state:stack (cons       the-last-element   (cdr state:stack )))
                            (set-cdr! state:stack (cons (list the-last-element ) (cdr state:stack ))))))
                       ((<   current-level state:current-level )
                        (display "ascend\n")
                        (set-cdr! state:stack (cddr state:stack)))
                       (else
                         (display "stay\n")))

                     (set! state:current-level  current-level)

                     (let ((ti-header (cadr state:stack)))

                       ; Check if it is a header.
                       (if (cdr
                             (or (assq 'is-header (car s4 ))
                                 (cons 'is-header #f )))
                         ; if the current element is a header,
                         (set-cdr! ti-header
                                   (cons 
                                     ; create a header element.
                                     (list 
                                       'header

                                       ; Set its header text on the first element.
                                       (list
                                         'text
                                         (cdr
                                           (or (assq 'text (car s4 ))
                                               (cons 'text #f )))))
                                     (cdr ti-header)))

                         ; if the current element is NOT a header
                         (set-cdr! ti-header
                                   ; set the string itself.
                                   (cons
                                     (list 
                                       'text
                                       (cdr
                                         (or (assq 'text (car s4 ))
                                             (cons 'text #f ))))
                                     (cdr ti-header)))))


                     ; go to the next element
                     (loop-4 (cdr s4))
                     ))))
             state:top-element))
         (phase-3 
           ; recursively reverses all of the elements in the tree.
           ; See COMMENT01
           (let loop-5 ((v5 (list phase-2))
                        (a5 '() ))
             (if (null? v5)
               '()
               (if (string? (car v5) )
                 ; if it is a string, then simply replicate it.
                 (append 
                   ; 1. go to the next element.
                   (loop-5 (cdr v5)
                           (cons (car v5) a5 ))
                   ; 2. the string value
                   (list   (car v5)))

                 ; if it is a pair, recursively loop to the both ways.
                 (append
                   ; 1. go to the next element.
                   (loop-5 (cdr v5)
                           (cons 
                             ; 2. reverse the pair on its car; skip the first element and reverse the others.
                             (cons (caar v5) 
                                   (loop-5 (cdar v5) '())) 
                             a5 ))
                   ; 2. reverse the pair on its car; skip the first element and reverse the others.
                   (list   (cons (caar v5) (loop-5 (cdar v5) '()))))
                 ))))
         (result phase-3))
    result))

; (display (disassemble text->list ))
(display
  ; (text->list "\t=hello\n" )  
  ; (text->list "\t=hello\n\t=world\n" )  
  ; (text->list "\t=hello\n\t=world\n\t\t=foo\n" )  
  ; (text->list "\t=hello\n\t=world\n\t\t=foo\n\t\t=bar\n" )  
  ; (text->list "\t=hello\n\tINTERNAL-TEXT\n\t=world\n\t\t=foo\n\t\t=bar\n" )  
  (text->list &<{readme.md} )  
  )
(newline)

;; ; COMMENT01 -- how to reverse a list in this program.
;; (display 
;;   (let loop ((e (list 0 1 2 3 4 5))) 
;;     (if (null? e)
;;       '()
;;       (append (loop (cdr e))
;;               (list (car e))))))
;; (newline)
;; 
;; 


; vim: expandtab :
