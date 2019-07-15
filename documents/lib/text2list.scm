(define-macro use-srfi (lambda (name)
                         `(import (srfi ,name))))

(use-srfi 1)
(use-srfi 13)
(use-srfi 14)

(define DEBUG #f)
(define (text->list s)
  (let* (
        ;
        (text->list:not-newline (char-set-complement (char-set #\newline )))
        (text->list:tab-size 8)

        ; proc
        (proc-parse 
          (lambda (arg)
            ; phase-1 : split the passed string by the line-break character `\n`
            ;           and count all leading blank spaces.
            (let loop-1 ((s1 (split-string arg #\newline ) #| (string-tokenize arg text->list:not-newline ) |# ))
              ; parsed-text
              (if (null? s1)
                '()
                (if (= 0 (string-length (string-trim-both (car s1)) ))
                  ;skip
                  (cons
                    (list 
                      (cons 'type 'parsed-text )
                      (cons 'text "" )
                      (cons 'indent-level #f)
                      (cons 'is-heading #f))

                    (loop-1 (cdr s1))
                    )
                  (cons 
                    (let-values ((( is-heading indent-level text ) 
                                  (let loop-2 ((s2 (string->list (car s1)))
                                               (count 0))
                                    ; count all leading space characters.
                                    (cond 
                                      ((null? s2)
                                       (values #f count "" ))
                                      ((eqv? (car s2) #\tab)
                                       (loop-2 (cdr s2 ) (+ count text->list:tab-size)))
                                      ((eqv? (car s2)  #\space)
                                       (loop-2 (cdr s2 ) (+ count 1                  )))
                                      ; treat leading '=' as the beginning of a paragraph 
                                      ((eqv? (car s2)  #\=)
                                       (values #t count 
                                               (string-trim-both
                                                 (list->string (cdr s2))))
                                       )
                                      ; treat leading '|' as the beginning of a line
                                      ((eqv? (car s2)  #\|)
                                       (values #f count 
                                               (if (< 1 (length s2))
                                                 (list->string 
                                                   (if (char-set-contains? char-set:whitespace (cadr s2))
                                                     (cddr s2)
                                                     (cdr s2)))
                                                 "    "
                                                 )))
                                      ; treat leading ';' as a comment line
                                      ((eqv? (car s2)  #\;)
                                       (values #f  #f "" )
                                       )
                                      (else
                                        (values #f count (list->string s2 )))))))
                                (if (= 0 (string-length text ))
                                  ; return #f if the current line is a blank line. BLANK_LINE
                                  #f
                                  (list 
                                    (cons 'type 'parsed-text )
                                    (cons 'text text )
                                    (cons 'indent-level indent-level)
                                    (cons 'is-heading is-heading))
                                  ))
                    (loop-1 (cdr s1))))))))

        (lookup-the-last-heading 
          (lambda (ti-heading)
            ; Look up the last added heading. Note that the
            ; headings and texts are stored on the list in
            ; reverse order.
            (let loop-lookup-the-last-heading ((e (cdr ti-heading)))
              (if (null? e)
                #f
                ; Check if the element points to the header of a heading object.
                (if (eq? 'heading (cdr (or (assq 'type (caar e))
                                           (cons 'type 'none))))
                  ; Then return the element.
                  (car e)
                  ; Otherwise, advance to the next element.
                  (loop-lookup-the-last-heading (cdr e)))))))
        (proc-main-loop 
          (lambda (arg)
            ; phase-2 :
            ; the main process. enumerate and process all lines.
            (let* ((parsed-text arg)
                   (state:top-element (list '((type . top))))
                   (state:stack       (list '((type . stack))))
                   (state:current-level -1 ))

              (if DEBUG (begin
                          (display parsed-text)
                          (newline)
                          ))
              ; push the root element to the stack
              (set-cdr! state:stack (cons state:top-element (cdr state:stack)))

              ; phase-4
              (let loop-4 ((s4 parsed-text))

                (if DEBUG (begin
                            (newline)
                            (display (format "(length state:stack) ~a\n" (length state:stack)))
                            (display (format "state:current-level ~a\n" state:current-level))
                            (display (format "(length (cadr state:stack) ) ~a\n" (length (cadr state:stack) )))
                            (display (format "(length state:top-element ) ~a\n" (length state:top-element )))
                            ))

                (if (null? s4)
                  (begin
                    (if DEBUG (begin (display "end\n")))
                    #f)
                  (if (not (car s4) )
                    ; if car s4 is #f, then ignore it. See BLANK_LINE .
                    (begin
                      (if DEBUG (begin (display "skip(blank line)\n")))
                      (loop-4 (cdr s4))
                      )
                    (let ((current-level
                            (cdr
                              (or (assq 'indent-level (car s4 ))
                                  (cons 'indent-level 0 )))))
                      (if DEBUG (begin (display (format "current-level ~a\n" current-level ) )))

                      ; this is done when it is the first time only.
                      (if (< state:current-level 0 )
                        (begin
                          ; if the first line is unluckily blank line, ignore it.
                          (if current-level 
                            (set! state:current-level current-level))

                          (if DEBUG (begin (display "initialized\n" )))
                          ))

                      ; Process the stack.
                      (if DEBUG (begin (display 'type) (display 
                                                         (cdr (or (assq 'type (car s4))
                                                                  (cons 'type 'none)))) (newline)))

                      (if (cdr (or (assq 'is-heading (car s4))
                                   (cons 'is-heading #f)))
                        ; then
                        (begin
                          ; Process the stack only when it is a heading;
                          ; otherwise, ignore the element.
                          (cond
                            ((not current-level)
                             (if DEBUG (begin (display "not current-level #t\n" )))
                             )
                            ((<                 state:current-level current-level )
                             (if DEBUG (begin (display "descend\n")))
                             ; push the last heading element of the heading elmenent on the stack
                             (let ((the-last-heading (lookup-the-last-heading (cadr state:stack))))
                               (if (pair? the-last-heading )
                                 (begin
                                   (if DEBUG (begin (display "push(pair) :") (display the-last-heading) (newline)))
                                   (set-cdr! state:stack (cons       the-last-heading   (cdr state:stack ))))
                                 ; (begin
                                 ;   (if DEBUG (begin (display "push(non pair) :") (display the-last-heading)    (newline)))
                                 ;   (set-cdr! state:stack (cons (list the-last-heading ) (cdr state:stack ))))
                                 )))
                            ((<=   current-level state:current-level )
                             (if DEBUG (begin
                                         (display "ascend\n")
                                         (display current-level)
                                         (newline)
                                         ))
                             (let loop-s ()
                               (if DEBUG (begin
                                           (display "pop" )
                                           (newline)
                                           (display "the heading on the top of the stack :" )
                                           (display (cadr state:stack ) )
                                           (newline)
                                           (display 'org-indent-level  )
                                           (display (cdr 
                                                      (or (assq 'org-indent-level (caadr state:stack ))
                                                          (cons 'org-indent-level -1))))
                                           (newline)
                                           ))

                               (if 
                                 (<= current-level
                                     (cdr 
                                       (or (assq 'org-indent-level (caadr state:stack ))
                                           (cons 'org-indent-level -1))))
                                 (begin
                                   (set-cdr! state:stack (cddr state:stack))
                                   (loop-s)))))
                            (else
                              (if DEBUG 
                                (display "stay\n"))))

                          ; preserve the value of current-level to the current state .
                          (if current-level
                            (set! state:current-level  current-level))
                          )
                        ; else
                        (begin
                          ; That is, the current element is a text node.
                          ; Push the last heading to the stack whenever it is found.
                          (let ((the-last-heading (lookup-the-last-heading (cadr state:stack))))
                            (if the-last-heading 
                              (begin
                                (if DEBUG (begin (display "push2(pair) :") (display the-last-heading) (newline)))
                                (set-cdr! state:stack (cons       the-last-heading   (cdr state:stack ))))))
                          ))


                      ; process the heading object.
                      (let ((ti-heading (cadr state:stack)))
                        (if (not current-level )
                          ;then 
                          (if state:current-level
                            ; When this came to #f, that means there was
                            ; unluckily a blank line right after a heading.
                            ; Just do nothing and ignore it.
                            ; Otherwise, create a single text node.

                            (set-cdr! ti-heading
                                      (cons 
                                        (list
                                          '((type . text)) 
                                          "")
                                        (cdr ti-heading))))


                          ;else
                          ; Check if it is a heading
                          (if (cdr
                                (or (assq 'is-heading (car s4 ))
                                    (cons 'is-heading #f )))
                            ; if the current element is a heading
                            (set-cdr! ti-heading
                                      (cons 
                                        ; create a heading element.
                                        (list 
                                          `((type . heading)
                                            (level . ,(- (length state:stack) 1 ) )
                                            (org-indent-level . ,current-level  ))

                                          ; Set its heading text on the first element.
                                          (list
                                            '((type . text)) 
                                            (cdr
                                              (or (assq 'text (car s4 ))
                                                  (cons 'text #f )))))
                                        (cdr ti-heading)))

                            ; if the current element is NOT a heading
                            (set-cdr! ti-heading
                                      ; set the string itself.
                                      (cons
                                        (list 
                                          `((type . text)
                                            (org-indent-level . ,current-level  ))
                                          (cdr
                                            (or (assq 'text (car s4 ))
                                                (cons 'text #f ))))
                                        (cdr ti-heading))))

                          )
                        )


                      ; go to the next element
                      (loop-4 (cdr s4))
                      ))))
              state:top-element)))

        (proc-correct-indent
          (lambda (arg)
            (letrec* ((get-minimum-indent-level 
                        (lambda (ti-heading)
                          ; Skip the first element since it is a text node for
                          ; a caption of  the heading. cdr -> cddr
                          (let loop ((e (cddr ti-heading))
                                     (min-value 10000))
                            (if (null? e)
                              (if (= min-value 10000) #f min-value )
                              (let ()
                                (if DEBUG (let() (display 'elem)(display e )(newline)))
                                (if (eq? 'text (cdr (or (assq 'type (caar e))
                                                        (cons 'type #f))))
                                  (let ((current-value (cdr (or (assq 'org-indent-level (caar e))
                                                                (cons 'org-indent-level 10000 )))))
                                    (if DEBUG (display (format "current-value min-value ~a ~a\n" current-value min-value )))
                                    (if (< current-value  min-value )
                                      (loop (cdr e) current-value)
                                      (loop (cdr e) min-value)))
                                  (loop (cdr e) min-value)))))))
                      (set-heading-indent
                        (lambda (ti-heading indent-level)
                          (if DEBUG (display (format "indent-level:  ~a\n" indent-level )))

                          (if indent-level 
                            ; then
                            (let loop ((e (cddr ti-heading)))
                              (if (null? e)
                                #f
                                (begin
                                  (if (eq? 'text (cdr (or (assq 'type (caar e))
                                                          (cons 'type #f))))

                                    (begin
                                      (if DEBUG (let() (display 'text )(display (car e)) (newline)) )
                                      (let loop2 ((e2 (cdar e)))
                                        (if (null? e2)
                                          #f
                                          (let ((org-indent-level 
                                                  (cdr (or (assq 'org-indent-level (caar e))
                                                           (cons 'org-indent-level #f )))))
                                            (if DEBUG (display (format "string:~a  lv:~a  org-lv:~a \n" (car e2) indent-level org-indent-level)))
                                            
                                            (if org-indent-level 
                                              (set-car! e2
                                                        (list->string
                                                          (append
                                                            (take (apply circular-list (list #\space ) ) 
                                                                  (- org-indent-level indent-level))
                                                            (string->list (car e2))))) )
                                            (loop2 (cdr e2)))))))
                                  (loop (cdr e)))))
                            ; else
                            )))
                      (proc-heading
                        (lambda (ti-heading)
                          (let loop ((e (cdr ti-heading)))
                            (if (null? e)
                              #f
                              (begin
                                (set-heading-indent 
                                  (car e ) 
                                  (get-minimum-indent-level 
                                    (car e)))
                                (loop (cdr e )))))

                          (let loop ((e (cdr ti-heading) ))
                            (if (null? e)
                              #f
                              (begin
                                (if (eq? 'heading (cdr (or (assq 'type (caar e))
                                                           (cons 'type #f))))
                                  (proc-heading (car e)))
                                (loop (cdr e)))))
                          )))
                     (if DEBUG (begin (display 'proc-heading )(display arg)(newline)))
                     (proc-heading arg)
                     arg
                     )))
        (proc-reverse-rec 
          (lambda (arg)
            ; recursively reverses all of the elements in the tree.
            ; See VERSION02 of COMMENT01 

            ; Car to strip the result because it is cupsulated by "(list arg)" below.
            ; I, at the time of writing this comment when it is one hour after the writing this block,
            ; already forgot why it is necessary. I admit that it is really complicated.
            (car 
              (let loop-5 ((v5 (list arg)) ; the current element
                           (a5 '() ))          ; the accumulator
                (if (null? v5)
                  a5
                  (if (string? (car v5) )
                    ; if it is a string, then simply replicate it.

                    (loop-5 
                      ; 1. go to the next element.
                      (cdr v5)
                      ; prepare for the accumulator
                      (cons 
                        ; 2. the string value
                        (car v5)
                        a5 ))

                    ; if it is a pair, recursively loop to the both ways.
                    (loop-5 
                      ; 1. go to the next element.
                      (cdr v5)
                      ; prepare for the accumulator
                      (cons 
                        ; 2. reverse the pair on its car; skip the first element and reverse the others.
                        (cons (caar v5) 
                              (loop-5 (cdar v5) '())) 
                        a5 ))
                    ))))))

        (proc-remove-tags
          (lambda (arg)
            ; remove 'org-indent-level from the result tree
            (letrec ((proc (lambda (header-elem)
                             (if (not (pair? header-elem))
                               ;then
                               header-elem
                               ;else
                               (begin
                                 (set-car!
                                   header-elem
                                   (alist-delete 'org-indent-level (car header-elem)))
                                 (let loop-6 ((v6 (cdr header-elem)))
                                   (if (null? v6)
                                     #f
                                     (begin
                                       (proc (car v6))
                                       (loop-6 (cdr v6)))))

                                 header-elem
                                 )))))
              (if DEBUG (begin
                          (newline)
                          (newline)
                          ))
              (proc arg)))))

    (letrec* ((phase-1 (proc-parse s))
              (phase-2 (proc-main-loop phase-1))
              (phase-3 (proc-reverse-rec phase-2))
              (phase-4 (proc-correct-indent phase-3))
              (phase-5 (proc-remove-tags phase-4) )
              (result phase-5))
             result)))

(define (split-string s d)
  (let ((in (string->list s) ))
    (reverse
      (let loop ((i in)
                 (str-start in)
                 (str-len    1)
                 (result '() ))
        (if (null? i )
          (let ((tmp (take str-start (-  str-len 1 ))))
            (if (null? tmp) 
              result
              (cons (list->string tmp )  result)))
          (if (eqv? (car i ) d )
            (loop 
              (cdr i)
              (cdr i)
              1
              (cons (list->string (take str-start (- str-len 1) )) result))
            (loop 
              (cdr i)
              str-start
              (+ str-len 1 )
              result)))))))

(define (get alist key default )
  (cdr (or (assq key alist )
           (cons key default))))

(define (list->md head )
  (letrec ((list->md:proc (lambda (head )
                            (if (pair? head)
                              ;then
                              (let ((result '()))
                                (let ((v (get (car head) 'type #f ) ))
                                  (cond
                                    ((eq? v 'heading )
                                     (set! result 
                                       (cons
                                         (let ((chlen (+ 1 (get (car head) 'level 0 ))))
                                           (string-append 
                                             (string-join
                                               (take (apply circular-list '("#") ) chlen ) "" )
                                             " "))
                                         result)))
                                    (else
                                      )))
                                (let loop ((v (cdr head)))
                                  (if (null? v)
                                    #f
                                    (begin
                                      (set! result
                                        (append
                                          (list->md:proc (car v))
                                          result))
                                      (loop (cdr v)))))
                                result)
                              ;else
                              (cond
                                ((string? head)
                                 (cons (string-append head "\n" ) '() ) )
                                (else
                                  (cons head '())))))))
    (string-join 
      (reverse (list->md:proc head))
      "")

    ))


(define (read-all . args )
  (let* ((port (if (null? args) 
                 (current-input-port) 
                 (car args))))
    (list->string
      (let loop ()
        (let ((c (read-char port)))
          (if (eof-object? c)
            '()
            (cons c (loop))))))))



; (define l1 (text->list "\t=hello\n" )  )
; (define l1 (text->list "\t=hello\n\t=world\n" )  )
; (define l1 (text->list "\t=hello\n\t=world\n\t\t=foo\n" )  )
; (define l1 (text->list "\t=hello\n\t=world\n\t\t=foo\n\t\t=bar\n" )  )
; (define l1 (text->list "\t=hello\n\tINTERNAL-TEXT\n\t=world\n\t\t=foo\n\t\t=bar\n" )  )
; (define l1 (text->list "\t=hello\n\tINTERNAL-TEXT\n\t=world\n\t\t=foo\n\t\t=bar\n\t=buzz\n" )  )


; (define l1 (text->list &<{readme.md} ))
; (define t1 (list->md l1 ))

; (display t1 )
; (newline)


; (display (disassemble text->list ))

(define l1 (text->list (read-all)))
(if DEBUG
  (begin
    (display l1)
    (newline)
    (newline)
    ))

(define t1 (list->md l1 ))
(display t1)


; (display
;   (string-join (split-string "a,b,c,,dd,d," #\, ) "," )
;   )

(newline)










;; ; COMMENT01 -- how to reverse a list in this program.
;;
;; ; VERSION01
;; (display 
;;   (let loop ((e (list 0 1 2 3 4 5))) 
;;     (if (null? e)
;;       '()
;;       (append (loop (cdr e))
;;               (list (car e))))))
;; (newline)
;; 
;; 
;;
;; ; VERSION02
;; (display 
;;   (let loop ((e  ls )
;;              (a '() ))
;;     (if (null? e)
;;       a
;;       (loop (cdr e) 
;;             (cons (car e) a )))))
;; (newline)

; vim: expandtab :
