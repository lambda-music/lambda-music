
; =======================================================
;
; ADDED (Fri, 11 Oct 2019 22:34:35 +0900)
; MOVED FROM `music.scm` (Sat, 06 Jun 2020 18:33:18 +0900)
;
; =======================================================
;
; make-song and other routines.
;
; =======================================================

(define (song-from next-label . args )
  (let ((limit-count (if (< 0 (length args)) 
                       (first args) 
                       1 ))
        (count 0 ))
    (lambda args
      (if (= (length args) 0 )
        ;then
        (begin 
          (set! count (+ count 1))
          (if (< count limit-count)
            (begin
              next-label)
            (begin
              #f)))
        ;else
        (let ((command (first args)))
          (cond
            ((eq? command 'reset )
             (set! count 0))
            (else
              (raise (format "unknown command ~a" command) )))
          #f)))))

(define (song-to next-label . args )
  (let ((limit-count (if (< 0 (length args)) 
                       (first args) 
                       1 ))
        (count 0 ))
    (lambda args
      (if (= (length args) 0 )
        ;then
        (begin 
          (set! count (+ count 1))
          (if (< count limit-count)
            (begin
              #f)
            (begin
              next-label)))
        ;else
        (let ((command (first args)))
          (cond
            ((eq? command 'reset )
             (set! count 0))
            (else
              (raise (format "unknown command ~a" command) )))
          #f)))))

(define (song-end . args )
  (let ((limit-count (if (< 0 (length args)) 
                       (first args) 
                       1 ))
        (count 0 ))
    (lambda args
      (if (= (length args) 0 )
        ;then
        (begin 
          (set! count (+ count 1))
          (if (< count limit-count)
            (begin
              (cons 'next #f))
            (begin
              (cons 'end #f ))))
        ;else
        (let ((command (first args)))
          (cond
            ((eq? command 'reset )
             (set! count 0))
            (else
              (raise (format "unknown command ~a" command) )))
          #f)))))

(define (song-head . args)
  (lambda args
    (if (= (length args) 0)
      ;then
      (begin
        (cons 'head 'repeat-last ))
      ; else
      (begin
        ; do nothing;
        #f))))

(define (song-reset . args)
  (lambda args
    (if (= (length args) 0)
      ;then
      (begin
        (cons 'reset 'repeat-last ))
      ; else
      (begin
        ; do nothing;
        #f))))

(define (song-conv song-data)
  (reverse (fold (lambda (x result)
                   (cond ((number? x)
                          (if (< 0 x )
                            (let ((lst (make-list (+ x 1) #f)))
                              (set-cdr! (last-pair lst) result )
                              lst)
                            (raise (format "~a is not a valid number." x ))))
                         (else 
                           (cons x result)))) '() song-data )))


; (song-conv (list 'hello #t 4  #t #f ))

(define (song-exec song-head init-song-current init-measure-length)

  (let loop-song ((song-next-pos init-song-current)
                  (measure-length init-measure-length ))

    (if (null? song-next-pos)
      ;then 
      (begin
        (display-warn 'loop-song )
        (newline-warn)
        ; *RETURN* When it comes to the end of song, automatically loop from the beginning.
        (loop-song song-head measure-length))
      ;else
      (begin
        (let ((song-next-val (car song-next-pos)))
          (cond
            ; === NOTATION DATA ===
            ((pair? song-next-val)
             ; *RETURN* return the current value
             (values 'command-none song-next-val (cdr song-next-pos) measure-length ))

            ; === COMMAND PROCEDURE ===
            ((procedure? song-next-val)
             (let* ((dummy #f)
                    ; get the procedure .
                    (proc          song-next-val)
                    ; execute the procedure .                          
                    (proc-val     (proc))

                    ; check for the default values and convert it to a pair. 
                    ; otherwise treat it as a locator and jump to there.                     
                    (proc-result  (cond
                                    ((pair? proc-val)
                                     ; do nothing
                                     proc-val)
                                    ((number? proc-val)
                                     (cons 'length proc-val))
                                    ((symbol? proc-val)
                                     (cons 'goto proc-val))

                                    ; if proc-val is #f, go to next;
                                    ((eq? proc-val #f )
                                     (cons 'next #f))
                                    (else
                                      (raise (format "an unsupported value was returned ~a" proc-val )))))

                    ; parse the proc-result
                    (proc-command          (car proc-result))
                    (proc-command-argument (cdr proc-result)))

               ; the car value of the proc-result is a command 
               ; and the cdr value of the proc-result is an argument.
               (cond
                 ; end command
                 ((eq? proc-command 'end )
                  (display-warn 'end-song )
                  (newline-warn)
                  ; *RETURN* return null for the next position to indicate the song is finished.
                  (values 'command-end (n (n type: 'len val: measure-length)(n type: 'quit)) '() measure-length ))
                 ; play command
                 ((eq? proc-command 'play )
                  ;*RETURN*  note that proc-command-argument could be 'repeat-last
                  (values 'command-none proc-command-argument (cdr song-next-pos) measure-length ))
                 ((eq? proc-command 'head )
                  ;*RETURN*
                  (values 'command-head proc-command-argument (cdr song-next-pos) measure-length ))
                 ((eq? proc-command 'reset )
                  ;*RETURN*
                  (values 'command-reset proc-command-argument (cdr song-next-pos) measure-length ))
                 ; goto command
                 ((eq? proc-command 'goto )
                  (let ((this-pair (memq proc-command-argument song-head )))
                    (if (eq? this-pair #f)
                      (raise (format "cannot find ~a" proc-result )))
                    (loop-song this-pair measure-length)))
                 ; length command
                 ((eq? proc-command 'length )
                  (loop-song (cdr song-next-pos) proc-command-argument ))
                 ; next command
                 ((eq? proc-command 'next )
                  (loop-song (cdr song-next-pos) measure-length)))))

            ; === LABEL ===
            ((symbol? song-next-val)
             ; check if it is one of special labels.
             (cond
               ; ((eq? 'end song-next-val ))
               (else
                 ; skip the label value.
                 (loop-song (cdr song-next-pos) measure-length ))
               ))
            ; === SKIP ===
            ((number? song-next-val)                   
             ; *RETURN* if the current value is a number, treat it as measure length 
             ; and return the current location.
             (set! measure-length song-next-val )
             (values 'command-none (n (n type: 'len val: measure-length)) (cdr song-next-pos) measure-length ))
            ((eq? song-next-val #f)
             ; *RETURN* return the current location
             (values 'command-none (n (n type: 'len val: measure-length)) (cdr song-next-pos) measure-length ))
            (else
              (raise (format "an unsupported value was found ~a" song-next-val )))))))))



; Append a pseudo element at the beginning.
; The song-exec procedure automatically reset the song list, when it
; reaches to the element at the beginning. If the song is programmed
; to jump to the element at the beginning, the automatic reset
; facility will be invoked unintentionally. To avoid this problem, it
; is necessary to append an extra element.

(define (make-song . args)  
  (let* ((song-command   #f )
         (notation-list  '())
         (song-head      (if (< 0 (length args)) (first  args) (raise "no song was specified" )))
         (song-current   (if (< 1 (length args)) (second args)  song-head ))
         (measure-length (if (< 2 (length args)) (third  args)  1         ))
         (reset-proc (lambda() 
                       (for-each (lambda(x)
                                   (if (procedure? x)
                                     (x 'reset )))
                                 song-head)))
         (head-proc (lambda() 
                      (set! song-current song-head))))


    (lambda args
      (if (= (length args) 0)

        ; then
        ; in case it requires to repeat we set everything inside a loop which name is "make-song-loop"
        (let make-song-loop ((a 0))
          ; return execute the song and return a notation-list . 
          (let-values ((( ret-song-command ret-notation-list ret-song-current ret-measure-length ) (song-exec song-head song-current measure-length)))

                      ; Command Check No.1
                      ; NOTICE!!!! THIS HAS TO BE DOCUMENTED. (Mon, 14 Oct 2019 04:10:30 +0900)
                      ; if symbol 'repeat-last is returned, it ask the routine to repeat the last notation-list.
                      (if (eq? ret-notation-list 'repeat-last)
                        (set! ret-notation-list notation-list))

                      (set! song-command   ret-song-command )
                      (set! notation-list  ret-notation-list )
                      (set! song-current   ret-song-current)
                      (set! measure-length ret-measure-length)                      

                      ; Command Check No.2
                      (cond 
                        ((eq? song-command 'command-reset)
                         (reset-proc)
                         ;repeat
                         (make-song-loop (+ a 1)))
                        ((eq? song-command 'command-head)
                         (head-proc)
                         ;repeat
                         (make-song-loop (+ a 1))))

                      ; *RETURN* return a notation-list .
                      notation-list))

        ; else
        ; execute special command
        (let ((command (first args)))
          (cond
            ((eq? command 'head)
             (head-proc))
            ((eq? command 'goto)
             (let ((goto-target (second args) ))
               (set! song-current (memq goto-target song-head))))
            ((eq? command 'reset)
             (head-proc)
             (reset-proc))
            (else
              (raise (format "unknown command ~a " command ))))
          ; *RETURN*
          #f)))))



; example
(if #f (putt (newt 'a 
                   ; This makes a song closure which plays the passed notation
                   ; lists sequencially. It has some functionarities to
                   ; implement label jumping.
                   (make-song 
                     ; List of Notation lists.
                     (list
                       ; This is a label.
                       'segno

                       ; A notation list
                       (n port: "fluidsynth" chan: 1 
                          (melody "do re mi fi")
                          (n type: 'putt id: 'hello2 styp: 'i pos: 0 
                             proc: (n port: "h2" chan: 0 (melody "do re mi")) )
                          (n type: 'len val: 2))

                       ; A notation list
                       (n port: "fluidsynth" chan: 1 
                          (melody "do re mi fi")
                          (n type: 'exec proc: (lambda() (display 'hello)(newline) )))

                       ; Creating a clousure to jump to the specified label.
                       (song-from 'segno)

                       'coda
                       ; A notation list to play coda.
                       (n port: "fluidsynth" chan: 1 
                          (melody "sol fi , mi , re , "))

                       ; A notation list to clean up.
                       (n (n type: 'remt id: 'hello   pos: 1 )
                          (n type: 'remt id: 'hello2  pos: 1 )
                          (n type: 'len val: 2)
                          )
                       )))))



