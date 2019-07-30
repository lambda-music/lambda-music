; 
; Pulsar-Sequencer written by Atsushi Oka 
; Copyright 2018 Atsushi Oka
; 
; This file is a part of Pulsar-Sequencer. 
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
 
(import (srfi 1))
(import (kawa pprint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tracing

; display-notes
(define display-notes (lambda (flg notes) 
                        (if flg
                          (begin
                            (set! *print-right-margin* 8000)
                            (display "(")
                            (for-each (lambda(note)
                                        (pprint note)
                                        (newline))
                                      notes)
                            (display ")")
                            (newline)
                            (newline)
                            ))

                        notes))

; display-args
(define display-args  (lambda args (map display args )))




; pretty stringification
(define (pstring e #!key ( margin 90 ) )
  (let* ((old-margin *print-right-margin* )
         (w  (java.io.StringWriter))
         (o (gnu.kawa.io.OutPort w #t #t)))
    (set! *print-right-margin* margin )
    (pprint e  o)
    (newline o)
    (set! *print-right-margin* old-margin )
    (o:flush)
    (w:toString)))



; SRFI 95: Sorting and Merging - Scheme Requests for Implementation
(import (srfi 95)) 

; SRFI 13: String Libraries - Scheme Requests for Implementation
(import (srfi 13))

(define (note-sorter a b)
  (let ((sa (symbol->string (car a)))
        (sb (symbol->string (car b)))
        )
    ; (display sb)
    ; (newline)
    (if (string= sa sb)
      #f
      (cond
       ((string= sa "type")
        #t)
       ((string= sb "type")
        #f)
       (else
        (string<  sa sb )
        )))))

(define (sort-note-properties note)
  (sort note note-sorter))

(define (cleanup-note-properties note)
  (delete-duplicates note 
                     (lambda (a b)
                       (eq? (car a) (car b)))))

 #|
  | Check if it is a list of associate lists or an associate list. 
  | Note that this examination presumes the first element of a note 
  | which is an associate list is '(type . note).  
  |
  | >> MODIFIED now it is not as above. (Tue, 30 Jul 2019 14:18:12 +0900)
  |#
; renamed note? -> notation? (Tue, 30 Jul 2019 14:29:43 +0900)
(define (notation? e)
  (and (pair? e)
       (pair? (car e ))
       (symbol? (caar e))
       (not (pair?   (cdar e)))))

; this function retrieves the note type value.
; ex)
; (get-notation-type '((type . note )( 1 2) ) )
; => note
(define (get-notation-type e)
  (if (not (notation? e))
    ; if the object is not a note, return #f.
    #f
    ; (raise  '( invalid-argument-error . "the passed object is not a note object.") )
    ; otherwise get the type value.
    (cdr (or (assq 'type e)
             (cons 'type #f)))))


; ((make-notation-type-checker 'note) '((type . note)(1 2)))
; => #t
(define (make-notation-type-checker type)
  (lambda (e) 
    (eq? type (get-notation-type e ))))


(define note? (make-notation-type-checker 'note))


(define debug-n-implementation #f)
(define (n-implementation append-proc . args)
  (if debug-n-implementation (begin
                               (display 'append-proc)
                               (display append-proc)
                               (newline)
                               (display 'args)
                               (display args)
                               (newline)))
  (let-values (((params notes) 
                (let loop ((idx 0)
                           (args args)
                           (params '())
                           (notes '()))
                  (if debug-n-implementation (begin
                                               (display 'append-proc-args)
                                               (display args)
                                               (newline)
                                               ))
                  (if (null? args)
                    (values params notes)
                    (let ((e (car args)))
                      (cond
                        ((keyword? e)
                         (set! args (cdr args))
                         (if (null? args)
                           (raise '(invalid-argument-error . "" ))
                           (let ((k (string->symbol (keyword->string e)))
                                 (v (car args)))
                             (loop (+ idx 1) (cdr args) (cons (cons k v) params)  notes))
                           ))
                        ((pair? e)
                         (if (notation? e)
                           (loop (+ idx 1 ) (cdr args) params (append-proc notes (list e)))
                           (loop (+ idx 1 ) (cdr args) params (append-proc notes       e))))
                        (else 
                          (cond
                            ((= idx 0)
                             (loop (+ idx 1) (cdr args) (cons (cons 'note e) params)  notes))
                            ((= idx 1)
                             (loop (+ idx 1) (cdr args) (cons (cons 'pos  e) params)  notes))
                            ((= idx 2)
                             (loop (+ idx 1) (cdr args) (cons (cons 'velo e) params)  notes))
                            (else
                              (raise '(invalid-argument-error . "" )))))))))))
              (if (= 0 (length notes))
                ; If no note was specified, the `params` object becomes a note object.
                (sort-note-properties 
                  (cleanup-note-properties 
                    (reverse params)))
                ; Otherwise, apply the params to the passed note objects.
                (begin
                  (map (lambda (note)
                         (sort-note-properties 
                           (cleanup-note-properties
                             (append 
                               (reverse params) 
                               note ))))
                       notes )))))

; === note ===
; "n" stands for "note" 

; 1. A way to shorten the creation of note data.
;    (n C4 0.1 0.3) => ((note . 60) (pos . 0.1) (velo . 0.1))
;   arg0 => note number
;   arg1 => position
;   arg2 => velocity
;
; 2. Bulk setting properties on the passed note data.
;    (n chan: 4 (n C4 0/4 0.1)
;               (n D4 1/4 0.1)
;               (n E4 2/4 0.1)
;               (n F4 3/4 0.1)) => (((chan . 4) (note . 60) (pos .  0/4) (velo . 0.1))
;                                   ((chan . 4) (note . 62) (pos .  1/4) (velo . 0.1))
;                                   ((chan . 4) (note . 64) (pos .  1/2) (velo . 0.1))
;                                   ((chan . 4) (note . 65) (pos .  3/4) (velo . 0.1)))


(define (n . args)
  (apply n-implementation (cons append args )))

; === parallel ===
; "p" stands for parallel processing for something I fogot.
; # This seems to be not used anywhere; you need to reconfirm that.
; (Sun, 28 Jul 2019 13:43:45 +0900)
(define (p . args)
  (apply n-implementation (cons append args )))

; === serial ===
; "s" stands for serial processing for something I fogot.
; (Sun, 28 Jul 2019 13:43:45 +0900)
(define (s . args)
  (apply append-notes args))


; from https://stackoverflow.com/questions/8028490/mapping-uneven-lists-help-scheme
(define (map-longest func filler . lists)
  (define (car-or-filler x)
    (if (pair? x) (car x) filler))
  (define (cdr-or-null x)
    (if (pair? x) (cdr x) '()))
  (unfold (lambda (lists)
            (every null? lists))
          (lambda (lists)
            (apply func (map car-or-filler lists)))
          (lambda (lists)
            (map cdr-or-null lists))
          lists))


; Added at (Sun, 28 Jul 2019 16:04:25 +0900)
; === map ===
; m!  
; The name of function "m" stands for mapping.
;
; The function m! takes a note list and a value list and set values on the
; notes of the specified property. This function modifies the passed note
; objects directly; therefore the name of this function postfixed with an
; exclamation mark.
;
; ex1) 
;  (m! '(((note . 60) (pos .  0/4) (velo . 0.0))
;        ((note . 62) (pos .  1/4) (velo . 0.0))
;        ((note . 64) (pos .  2/4)             )
;        ((note . 65) (pos .  3/4) (velo . 0.0)))
;      velo: '( 0.1 0.2 0.3 0.4 )
;      pos : '( 0/8 1/8 2/8 3/8 ))              
; ;
; =>  (((note . 60) (pos .  0/8) (velo . 0.1))
;      ((note . 62) (pos .  1/8) (velo . 0.2))
;      ((note . 64) (pos .  2/8) (velo . 0.3))
;      ((note . 65) (pos .  3/8) (velo . 0.4)))
;
; ex2) 
; (m! 
;   (m! #f pos: '( 1 2 3 ))
;   pos: (lambda (v)
;          (display v)
;          (newline)
;          (+ v 1 ) ))
;
; => (((pos . 2)) ((pos . 3)) ((pos . 4)))
;
; === TEST ===
;  (display-notes #t (m! (copy-cons '(((note . 60) (pos .  0/4) (velo . 0.0))
;                                    ((note . 62) (pos .  1/4) (velo . 0.0))
;                                    ((note . 64) (pos .  2/4)             )
;                                    ((note . 65) (pos .  3/4) (velo . 0.0))))
;
;                       velo: '( 0.1 0.2 0.3 0.4 )
;                       pos:  '( 0/8 1/8 2/8 3/8 )))
;                                        => (((note . 60) (pos . 0)   (velo . 0.1))
;                                            ((note . 62) (pos . 1/8) (velo . 0.2))
;                                            ((note . 64) (pos . 1/4) (velo . 0.3))
;                                            ((note . 65) (pos . 3/8) (velo . 0.4))) 


(define (m! . args)
  (let ((lst-of-alst (car args ))
        (args        (cdr args )))

    (if (number? lst-of-alst )
      (set! lst-of-alst (make-list lst-of-alst '())))

    (let loop ((args args))
      (if (<= 2 (length args) ) 
        ;then
        (let ((k    (car  args))
              (v    (cadr args))
              (args (cddr args)))

          (if (and (keyword? k ))
            ;then
            (begin
              (cond
                ((list? v)
                 (let ((kwd-val   (string->symbol (keyword->string k )))
                       (lst-of-val v ))
                   (set! lst-of-alst 
                     (map-longest (lambda (alst cur-val) (if cur-val 
                                      ;then
                                      (begin
                                        (if (not alst ) 
                                          (set! alst '()))
                                        (let ((alst alst))
                                          (set-cdr!
                                            ; set the current value to the cons-cell.
                                            (or (assq kwd-val alst)
                                                ; create a new cons cell if not found
                                                (let ((new-cell (cons kwd-val #f)))
                                                  ; if the specific association element was not
                                                  ; found, append the newly created cell to the
                                                  ; alst. Additionally recreate alst.
                                                  (set! alst (append alst (list new-cell)))
                                                  new-cell))
                                            cur-val)
                                          alst ))
                                      ;else
                                      (begin
                                        alst)))
                                  #f
                                  lst-of-alst
                                  lst-of-val ))
                   ))
                ((procedure? v) 
                 (let ((kwd-val   (string->symbol (keyword->string k )))
                       (map-proc v ))
                   (set! lst-of-alst 
                     (map (lambda (alst)
                            (let* ((the-cell (or (assq kwd-val alst)
                                                 ; create a new cons cell if not found
                                                 (let ((new-cell (cons kwd-val #f)))
                                                   ; if the specific association element was not
                                                   ; found, append the newly created cell to the
                                                   ; alst. Additionally recreate alst.
                                                   (set! alst (append alst (list new-cell)))
                                                   new-cell)))
                                   (the-cell-value (map-proc (cdr the-cell)) ))

                              (set-cdr!
                                the-cell
                                ; set the current value to the cons-cell.
                                the-cell-value))
                            alst
                            )
                          lst-of-alst
                          ))))
                (else 
                  (raise 'invalid-argument-error-1))))
            ;else
            (raise 'invalid-argument-error-2))
          (loop args))
        ;else
        (values)))

    lst-of-alst))

; (display-notes #t (m! (copy-cons '(((note . 60) (pos .  0/4) (velo . 0.0))
;                                    ((note . 62) (pos .  1/4) (velo . 0.0))
;                                    ((note . 64) (pos .  2/4)             )
;                                    ((note . 65) (pos .  3/4) (velo . 0.0))))
;                       velo: '( 0.1 0.2 0.3 0.4 )
;                       pos:  '( 0/8 1/8 2/8 3/8 )))
; 


; The name of the function "ap" stands for arithmetic progression.
; ex)
;   (ap 4 1 )=> '( 0/4 1/4 2/4 3/4 )
;   (ap 3 4 )=> '( 0/4 1/4 2/4 3/4 )
;   (ap 3 1 )=> '( 0/3 1/3 2/3 )
;   (ap 3 2 )=> '( 0 2/3 4/3 )
;   (ap 4 2 )=> '( 0 1/2 1 3/2 )
;
; This function maybe useful for creating polyrhythmic patterns.

(define (ap n l)
  (fold-right (lambda (x y) (cons (* (/ x n ) l )  y)) '() (iota n)))




; Currently this funciton is not used.
; (Sun, 28 Jul 2019 13:43:45 +0900)
(define (nmap proc . args)
  (let ((total-count (length args)))
    (let loop ((args args)
               (index 0))
      (if (null? args)
        '()
        (cons
          (proc (car args) index total-count)
          (loop (cdr args) (+ index 1)))
        ))))

; "cl" stands for circular-list
; This function is shorthand for srfi-1's default circular-list function.
(define (cl lst ) 
  (if (null? lst)
    (raise (cons 'invalid-argument-exception "arg0 cannot be null" ))
    (let (( lst2 (list-copy lst) ))
      (set-cdr! (last-pair lst2 ) lst2 )
      lst2)))


; "nc" stands for note-count.
; This function counts notes in the passed note data.
; ex)
;     (nc (melody '(do re mi end )) )
;        => 3
;     (nc (melody '(do re mi fa sol end )) )
;        => 5
(define (nc lst)
  (fold (lambda (alst count)
          (if (eq? 'note
                   (cdr (or (assq 'type alst )
                            (cons 'type #f   ))))
            (+ count 1 )
            (+ count 0 )))
        0
        lst ))


; "lm" stands for "limit".
; lm function limit the length of the passed list;
; if the length of the list exceeds the number
; that is passed as count parameter, this returns
; (take lst count) .
; Otherwise this returns the passed list.
; (lm 5 '(0 1 2 3 ) )
;    => (0 1 2 3 ) 
; (lm 2 '(0 1 2 3 ) )
;    => (0 1 ) 
;
; Added (Mon, 29 Jul 2019 16:13:44 +0900)
;

(define (lm count lst)
  (if (<= (length lst) count )
    lst
    (take lst count)))


; This function performs deep copy on a cons cell.
; Currently this funciton is not used.
; (Sun, 28 Jul 2019 13:43:45 +0900)
(define (copy-cons c)
  (if (pair? c)
    (cons 
     (copy-cons (car c)) 
     (copy-cons (cdr c)))
    c))

; === tie ===
; "t" stands for "tie"
; This seems to be there for connecting notes which are placed next to each
; other. For further information, I forgot.
; (Sun, 28 Jul 2019 13:48:04 +0900)
(define (t . args) 
  (set! args (copy-cons args))
  (let ((notes (reverse (let loop (( args args))
                          (if (null? args)
                            '()      
                            (if (notation? (car args))
                              (cons (car args) (loop (cdr args)))
                              (append (car args) (loop (cdr args)))))))))
    (reverse
      (let loop ((notes notes)
                 (tie-pos #f)
                 (tie-len #f)
                 (tie-obj-pos #f)
                 )
        (if (null? notes)
          '()
          ; If the current note is a tie, do this.
          (if (cdr (or (assq 'tie (car notes)) '(tie . #f)))
            ; If tie-pos is already set, ignore the tie.
            (if tie-pos
              ; Ignore the tie.
              ; (cons (car notes) (loop (cdr notes) tie-pos #f #f))
              ; Do not cons. Because we need to delete the current note.
              (loop (cdr notes) tie-pos tie-len #f)

              ; Otherwise set the pos to the tie-pos
              (let ((pos (cdr (or (assq 'pos (car notes)) '(pos . #f))))
                    (len (cdr (or (assq 'len (car notes)) '(len . #f))))
                    )
                ; check error 1
                (if (not pos) (raise '("a tie without pos value is not allowed." . error )))             

                ; check error 2
                (if (not len) (raise '("a tie without len value is not allowed." . error )))             

                ; Set the pos to the tie-pos.
                ; Do not cons. Because we need to delete the current tying  note.
                (loop (cdr notes) pos len #f)))

            ; If the current note is NOT a tie, do this  
            (if (not tie-pos)
              ;If no tying note is found so far, continue the process.
              (cons (car notes) (loop (cdr notes) #f #f #f ))
              
              ; Otherwise process the note as tied.
              (let* ((pos (cdr (or (assq 'pos (car notes)) '(pos . #f))))
                     (curr-tie-obj-pos (or tie-obj-pos
                                           ; If the tie-obj-pos is not set, set the pos on tie-obj-pos
                                           ; That is, this note becomes the first tied note.
                                           (if pos
                                             pos
                                             (raise '("a note without pos value is tied." . error ))))))               
                #|
                | (If the tie-obj-pos equals to the pos, that means,
                      |     (1. The note is the first note of a note sequence. )
                      |     (2. Or the note is one of notes that  following the first note.))
                |#
                (if (eqv? curr-tie-obj-pos pos )
                  (begin
                    (display 'tie-pos\ )(display  tie-pos )
                    (newline)
                    (display 'tie-len\  )(display tie-len )
                    (newline)
                    (display 'curr-tie-obj-pos\  )(display curr-tie-obj-pos )
                    (newline)
                    (set-cdr!
                      (or (assq 'len (car notes))
                          ; If len is not found, create it.
                          (let ((new-pair (cons 'len #f)))
                            ; Note that this directly modifies the note.
                            (set-cdr! (last-pair (car notes))
                                      (cons new-pair '()))
                            ; Return the created new pair.
                            new-pair))

                      ; Calculate the length of the tied note and set to 
                      ; cdr of the cons cell.

                      (+ (- tie-pos curr-tie-obj-pos ) tie-len))

                    ; Process the next note.         
                    (cons (car notes) (loop (cdr notes) tie-pos tie-len curr-tie-obj-pos ))        
                    )
                  ; This note is the note which is next to the note sequence.
                  ; Note that this object will never be a tie note because if 
                  ; so the process will note come into this.
                  (begin
                    (cons (car notes) (loop (cdr notes) #f #f #f ))))))

            ))))))

#|
        ;Test
        (t
          '(
            ((type . note)(pos . 0.01 ))
            ((type . note)(pos . 0.02 )))
          '((type . note)(pos . 0.1 ))
          '((type . note)(pos . 0.2 ))
          '((type . note)(pos . 0.21 ))
          '((type . note)(pos . 0.22 ))
          '((type . note)(pos . 0.23 )(tied . #t))
          '(
            ((type . note)(pos . 0.3 ))
            ((type . note)(pos . 0.4 )))
          )
        (t
          '((type . note) (pos . 0.5 )(note . 1))
          '((type . note) (pos . 1 )(note . 1))
          '((type . note) (pos . 1 )(note . 2))
          '((type . note) (pos . 2 )(len . 1 )(tie . #t))
          '((type . note) (pos . 3 )(len . 1 )(tie . #t))
          )
|#

(define (cor-proc-1 lst)
  (if (list? lst )
    (let loop ((lst lst)) 
      (if (null? lst)
        '()
        (let ((e (car lst)))
          (if (list? e)
            (cond
              ((eq? (car e) 'quote)
               (cons '\' (cons (cor-proc (cadr e)) (loop (cdr lst)))))
              ((eq? (car e) 'unquote)
               (cons '\, (cons (cor-proc (cadr e)) (loop (cdr lst)))))
              (else
                (cons (cor-proc e ) (loop (cdr lst)))))        
            (cons       e  (loop (cdr lst)))))))
    ; if the passed object is not a list, return it.
    ; Note that cor-proc is called recursively even when it is not a list.
    lst))

(define (cor-proc lst)
  (if (list? lst )
    (let loop ((lst lst)) 
      (if (null? lst)
        '()
        (let ((e (car lst)))
          (if (list? e)
            (append 
              (cor-proc e )
              (loop (cdr lst)))
            (cons   e (loop (cdr lst)))))))
    ; if the passed object is not a list, return it.
    ; Note that cor-proc is called recursively even when it is not a list.
    lst))


(define (cor lst)
  (let ((result (cor-proc lst)))
    (if (not (eq? (last result) 'end))
      (raise "Missing 'end element. The last element must be 'end "))

    (drop-right result 1)))
  
; cor test 1
; (cor '( c'16 e,8 ( test c d e 'd 'e ) ' ( a b c 'e  ' end ) ) )
; cor test 2
; cor '( do re mi fa sol 5 la 2 ti 8 d ' end  ))
; cor test 3 ( recursive )
; (cor '( o 1 k 4 do ' re , , end ))
; (cor '( a b ( a b ( c d e ( f g ) ) )  c , end ) )



(define note-names
  `(
    (dae . ((interval . ,(+ -3  0))))
    (daw . ((interval . ,(+ -2  0))))
    (de  . ((interval . ,(+ -1  0))))
    (do  . ((interval . ,(+  0  0))))
    (di  . ((interval . ,(+  1  0))))
    (dai . ((interval . ,(+  2  0))))
    (dao . ((interval . ,(+  3  0))))

    (rae . ((interval . ,(+ -3  2))))
    (raw . ((interval . ,(+ -2  2))))
    (ra  . ((interval . ,(+ -1  2))))
    (re  . ((interval . ,(+  0  2))))
    (ri  . ((interval . ,(+  1  2))))
    (rai . ((interval . ,(+  2  2))))
    (rao . ((interval . ,(+  3  2))))

    (mae . ((interval . ,(+ -3  4))))
    (maw . ((interval . ,(+ -2  4))))
    (me  . ((interval . ,(+ -1  4))))
    (mi  . ((interval . ,(+  0  4))))
    (ma  . ((interval . ,(+  1  4))))
    (mai . ((interval . ,(+  2  4))))
    (mao . ((interval . ,(+  3  4))))

    (fae . ((interval . ,(+ -3  5))))
    (faw . ((interval . ,(+ -2  5))))
    (fe  . ((interval . ,(+ -1  5))))
    (fa  . ((interval . ,(+  0  5))))
    (fi  . ((interval . ,(+  1  5))))
    (fai . ((interval . ,(+  2  5))))
    (fao . ((interval . ,(+  3  5))))

    (sae . ((interval . ,(+ -3  7))))
    (saw . ((interval . ,(+ -2  7))))
    (se  . ((interval . ,(+ -1  7))))
    (sol . ((interval . ,(+  0  7))))
    (si  . ((interval . ,(+  1  7))))
    (sai . ((interval . ,(+  2  7))))
    (sao . ((interval . ,(+  3  7))))

    (lae . ((interval . ,(+ -3  9))))
    (law . ((interval . ,(+ -2  9))))
    (le  . ((interval . ,(+ -1  9))))
    (la  . ((interval . ,(+  0  9))))
    (li  . ((interval . ,(+  1  9))))
    (lai . ((interval . ,(+  2  9))))
    (lao . ((interval . ,(+  3  9))))

    (tae . ((interval . ,(+ -3 11))))
    (taw . ((interval . ,(+ -2 11))))
    (te  . ((interval . ,(+ -1 11))))
    (ti  . ((interval . ,(+  0 11))))
    (ta  . ((interval . ,(+  1 11))))
    (tai . ((interval . ,(+  2 11))))
    (tao . ((interval . ,(+  3 11))))
    ))
#|
 | a 'tmp-parser-note is a temporary virtual note which is used only in this
 | parser. The 'tmp-parser-note 's are processed afterwards, then merged
 | into notes which are next to the 'tmp-parser-note and removed.
 |
 | See the code below.
 |#


(define debug-parse-notes #f)

(define (parse-notes notes)
  #|
   | Define common methods which will be placed on a result-note .
   |     - op operand ... this is usually a result-note object. 
   |     - vl value   ... this is usually a 'tmp-parser-note  object
  |#
  (let ((proc-merge (lambda(op  vl)
                      (append op
                              ; delete the type node in the msg note.
                              (alist-delete 'type vl eq?))
                      ))
        (proc-add   (lambda(op  vl)
                      ; Delete the type node in the msg note.
                      (let ((vl (alist-delete 'type vl eq?))
                            (p (or (assq 'tmp-len op)
                                   (cons 'tmp-len 0)))
                            )
                        (if debug-parse-notes (begin
                                                (display p)
                                                (newline)
                                                ))

                        ; Sum values of 'tmp-len then set it to the cell.
                        (set-cdr! p 
                                  (fold (lambda(e val)
                                          (if (eq? 'tmp-len (car e))
                                            (+ val (cdr e))
                                            val)
                                          ) 
                                        (cdr p) 
                                        vl))

                        ; Replace 'tmp-len pair with the pair and return it.
                        (append
                          (alist-delete 'tmp-len op eq?)
                          (list p))
                        )))
        )
    ; There are two main loop here.

    ; === The first loop ===
    (let loop ((notes notes)
               (transpose  0)
               (octave     4))
      (if (null? notes)
        '()
        (cond
          ; octave specifier
          ((eq? (car notes) 'o )
           (if (number? (cadr notes) )
             (cons
               (list
                 (cons 'type 'tmp-octave )
                 (cons 'value (cadr notes)))
               (loop (cddr notes) transpose (cadr notes)))
             (raise "an invalid octave specifier")))

          ; transpose specifier (not implemented yet)
          ((eq? (car notes) 'k )
           (if (number? (cadr notes) )
             (cons
               (list
                 (cons 'type 'tmp-transpose )
                 (cons 'value (cadr notes)))
               (loop (cddr notes) (cadr notes) octave ))
             (raise "an invalid transpose specifier")))

          ; quote ( ' )  treat it as a direction specifier.
          ((eq? (car notes) 'quote )
           (cons
             (list
               (cons 'type 'tmp-parser-note )
               (cons 'tmp-direction 1 ))

             (loop (cdr notes) transpose octave )))

          ; unquote ( , )  treat it as a direction specifier.
          ((eq? (car notes) 'unquote )
           (cons
             (list
               (cons 'type 'tmp-parser-note )
               (cons 'tmp-direction -1 ))

             (loop (cdr notes) transpose octave )))
          ((number? (car notes) )
           (cons
             (list
               (cons 'type 'tmp-parser-note )
               (cons 'tmp-len (car notes) ))

             (loop (cdr notes) transpose octave ))
           )

          ; ; rest
          ; ((eq? 'r (car notes) )
          ;  (cons
          ;    (list
          ;      (cons 'type 'tmp-rest ))

          ;    (loop (cdr notes) transpose octave ))
          ;  )
          ; ; sustain
          ; ((eq? 's (car notes) )
          ;  (cons
          ;    (list
          ;      (cons 'type 'tmp-sus-r ))

          ;    (loop (cdr notes) transpose octave ))
          ;  )
          (else 
            (let ((result-note 
                    ; creating a note
                    (cond
                      ; rest
                      ((eq? 'r (car notes))
                       (list
                         (cons 'type 'tmp-rest )
                         ; (cons 'proc proc-add )
                         ))

                      ; sustain (rational)
                      ((or (eq? 's  (car notes))
                           (eq? 'sr (car notes)))
                       (list
                         (cons 'type 'tmp-sus-r )
                         ; (cons 'proc proc-add )
                         ))

                      ; sustain (constant)
                      ((eq? 'sc (car notes))
                       (list
                         (cons 'type 'tmp-sus-c )
                         ; (cons 'proc proc-add )
                         ))
                      ; tie
                      ((eq? '- (car notes))
                       (list
                         (cons 'type 'tmp-tie )
                         (cons 'tie #t)
                         ; (cons 'proc proc-add )
                         ))

                      ; chord on
                      ((eq? '< (car notes))
                       (list
                         (cons 'type 'tmp-chord-on )
                         ; (cons 'proc proc-add )
                         ))

                      ; chord off
                      ((eq? '> (car notes))
                       (list
                         (cons 'type 'tmp-chord-off )
                         ; (cons 'proc proc-add )
                         ))

                      ; note
                      (else
                        (let ((note-pair 
                                (or
                                  (assq (car notes) note-names)
                                  (raise (string-append "an invalid note name "
                                                        (format "~a" (car notes)))))))
                          (list
                            (cons 'type 'note )
                            ; (cons 'proc proc-add)
                            (cons 'tmp-interval  
                                  (cdr (assq 'interval (cdr note-pair))))
                            ; (cons 'tmp-transpose transpose)
                            ; (cons 'tmp-octave    octave)
                            )
                          )))))

              ; Note that this is a double recursive call
              ; which processes the result of a future call.
              (let ((result 
                      (loop (cdr notes) transpose octave )))

                (if debug-parse-notes (begin
                                        (display 'begin-loop-result)
                                        (newline)))

                ; === The second loop ===
                (let loop-result ((result result)
                                  (result-note result-note ))

                  (if (null? result)
                    ; end of the loop-result
                    (begin
                      (if debug-parse-notes
                        (begin
                          (display 'loop-end1)
                          (display result)
                          (newline)))

                      (cons result-note result))

                    ; check if the result contains a special note
                    ; which is called 'msg' note, 
                    (let ((notation-type (cdr (or
                                            (assq 'type (car result))
                                            (cons 'type #f)))))
                      (if debug-parse-notes (begin
                                              (display (format "notation-type  : ~a " notation-type ))
                                              (newline)))

                      (if (eq? notation-type 'tmp-parser-note)
                        ; while the result contains a msg note,
                        ; merge it to the result note.
                        (begin
                          (if debug-parse-notes (begin
                                                  (display 'loop)
                                                  (display result)
                                                  (newline)
                                                  (display 'loop-result-note)
                                                  (display result-note)
                                                  (newline)))

                          ; Go to the next element.
                          (let ((tmp (loop-result 
                                       (cdr result)
                                       (append result-note 
                                               ; delete the type node in the msg note.
                                               (alist-delete 'type (car result) eq?))
                                       ; (apply
                                       ;   (cdr (or (assq 'proc result-note)
                                       ;            (lambda args (raise "internal error : no proc was found" ))))
                                       ;   (list
                                       ;     result-note
                                       ;     (car result)))
                                       )))
                            (if debug-parse-notes (begin
                                                    (display 'loop-result)
                                                    (display tmp)
                                                    (newline)))
                            tmp))

                        ; otherwise exit from the loop and return the result
                        (begin
                          (if debug-parse-notes (begin
                                                  (display 'loop-end2)
                                                  (display result)
                                                  (newline)))
                          ; Do not go to the next element.
                          (cons result-note result))

                        ))))
                ))

            ;; experimentally convert it to a number
            ;(let ((n (string->number (symbol->string (car notes))) ))
            ;  (if n 
            ;    ; if the value is a number
            ;    (cons
            ;      (list
            ;        (cons 'type 'tmp-len )
            ;        (cons 'value n ))

            ;      (loop (cdr notes) transpose octave ))

            ;    ; if the value is a string other than a number.



            ;    ))

            ))))))


(import (kawa regex))
(require 'regex )

(define debug-translate-notes #f)
(define (translate-notes in-notes) 
  (let ((result in-notes ))

    ; sum all tmp-direction values on the cons cells.
    (if #f
      (set! result (let loop ((notes result))
                     (if (null? notes)
                       '()
                       ; if the element is a note, 
                       (if (eq? 'note (cdr (or (assq 'type (car notes) )
                                               (cons 'type #f))))
                         (begin
                           (cons
                             (let ((summed-direction #f))
                               (append (filter-map (lambda (e)
                                                     (if (eq? (car e) 'tmp-direction)
                                                       (begin
                                                         (set! summed-direction 
                                                           (if summed-direction
                                                             (+ summed-direction (cdr e ))
                                                             (cdr e )
                                                             ))
                                                         #f)
                                                       (begin
                                                         e))) (car notes))
                                       (list
                                         (cons 'tmp-direction summed-direction))))
                             (loop (cdr notes))))
                         (begin
                           (cons
                             (car notes)
                             (loop (cdr notes)))))))))

    (if debug-translate-notes (begin
                                (display 'result0)
                                (display result)
                                (newline)
                                ))

    (let ((sub-proc (lambda (key-name in-notes)
                      (let loop ((notes in-notes))
                        (if (null? notes)
                          '()
                          (cons
                            (let ((note (car notes)))
                              (if debug-translate-notes (begin
                                                          (display 'notes00)
                                                          (display note)
                                                          (newline)
                                                          ))
                              (if (eq? 'note (cdr (or (assq 'type note )
                                                      (cons 'type #f))))
                                ; Recreate a note
                                (let ((sum 
                                        (fold (lambda (e sum)
                                                (if (eq? (car e) key-name )
                                                  (+ (cdr e ) (if sum sum 0))
                                                  sum))
                                              #f 
                                              note))
                                      (modified-note
                                        (filter (lambda (e)  
                                                  (not (eq? (car e) key-name )))
                                                note)))
                                  ; if no key-name was found, `sum` will be #f; 
                                  ; otherwise `sum` will be a number.
                                  (if sum 
                                    (append 
                                      modified-note
                                      (list (cons key-name sum )))

                                    modified-note))
                                ; do nothing and return the note 
                                note ))
                            (loop (cdr notes))))))))
      (set! result (sub-proc 'tmp-len       result))
      ; (set! result (sub-proc 'tmp-direction result))
      )

    (if debug-translate-notes (begin
                                (display 'result1)
                                (display result)
                                (newline)
                                ))

    ; calculate note values
    (set! result (let* ((state-octave              4)
                        (state-interval           #f)
                        (state-direction           1)
                        (state-transpose           0)
                        (state-position            0)
                        (state-len               1/4)
                        (state-sustain           4/4)
                        (sustain-mode-rational (lambda(len) (* len state-sustain)))
                        (sustain-mode-constant (lambda(len)        state-sustain ))
                        (state-sustain-mode      sustain-mode-rational )
                        (state-velocity          3/5)
                        (state-chord-mode         #f)
                        (state-chord-mode-stack '() )
                        (state-chord-mode-push (lambda(e)
                                                 (set! state-chord-mode-stack
                                                   (cons e state-chord-mode-stack))))
                        (state-chord-mode-pop (lambda()
                                                (let ((e (car state-chord-mode-stack)))
                                                  (set! state-chord-mode-stack
                                                    (cdr state-chord-mode-stack))
                                                  e)))
                        (state-chord-mode-empty (lambda()
                                                  (null? state-chord-mode-stack)))
                        )

                   (let loop ((notes result))
                     (if debug-translate-notes (begin
                                                 (display 'state-loop)
                                                 (newline)))
                     (if (null? notes)
                       ; end of the loop
                       (cons
                         (list (cons 'type  'len )
                               ; CAUTION : 'val not 'value (Tue, 23 Oct 2018 14:25:27 +0900)
                               (cons 'val state-position ))
                         '())

                       ; the loop proc
                       (let ((type 
                               (cdr (or (assq 'type (car notes))
                                        (cons 'type #f))) ))
                         (cond
                           ; octave
                           ((eq? type 'tmp-octave )
                            (if debug-translate-notes (begin
                                                        (display 'tmp-octave  )
                                                        (display (car notes))
                                                        (newline)))

                            (set! state-octave
                              (cdr (assq 'value (car notes))))

                            ; delete the current tmp note
                            (loop (cdr notes)))

                           ; transpose
                           ((eq? type 'tmp-transpose )
                            (set! state-transpose
                              (cdr (assq 'value (car notes))))

                            ; delete the current tmp note
                            (loop (cdr notes)))

                           ; length (deprecated)
                           ((eq? type 'tmp-len )
                            (set! state-len
                              (cdr (assq 'value (car notes))))

                            ; delete the current tmp note
                            (loop (cdr notes)))

                           ; sustain (rational)
                           ((eq? type 'tmp-sus-r )
                            (if debug-translate-notes (begin
                                                        (display 'tmp-sus-r)
                                                        (display (car notes))
                                                        (newline)))
                            (set! state-sustain
                              (cdr (or
                                     (assq 'tmp-len (car notes))
                                     (cons 'tmp-len state-sustain ))))

                            (set! state-sustain-mode sustain-mode-rational)

                            ; delete the current tmp note
                            (loop (cdr notes)))

                           ; sustain (constant)
                           ((eq? type 'tmp-sus-c )
                            (if debug-translate-notes (begin
                                                        (display 'tmp-sus-c)
                                                        (display (car notes))))
                            (newline)
                            (set! state-sustain
                              (cdr (or
                                     (assq 'tmp-len (car notes))
                                     (cons 'tmp-len state-sustain ))))

                            (set! state-sustain-mode sustain-mode-constant)

                            ; delete the current tmp note
                            (loop (cdr notes)))

                           ; chord on
                           ((eq? type 'tmp-chord-on )
                            (set! state-chord-mode #t)

                            ; delete the current tmp note
                            (loop (cdr notes)))
                           ; tie / rest and chord off
                           ((or 
                              (eq? type 'tmp-tie )
                              (eq? type 'tmp-rest )
                              ; because chord off implies let the cursor go forward .
                              (eq? type 'tmp-chord-off )
                              )

                            (if debug-translate-notes (begin
                                                        (display 'tmp-tie)
                                                        (display (car notes))
                                                        (newline)))

                            (if (eq? type 'tmp-chord-off )
                              (set! state-chord-mode #f))

                            (let ((position state-position)
                                  (len
                                    (cdr (or
                                           (assq 'tmp-len (car notes))
                                           (cons 'tmp-len state-len)))))

                              (if debug-translate-notes (begin
                                                          (display (format "tie len: ~a\n" len))
                                                          (newline)))

                              ; Set the current length to the state object.
                              (set! state-len len)

                              ; Advance the current position
                              (if (not state-chord-mode)
                                (set! state-position (+ state-position  len)))


                              (if (eq? type 'tmp-chord-off )
                                (let chord-loop ()
                                  (if (not (state-chord-mode-empty))
                                    (let* ((e (state-chord-mode-pop))
                                           (len-cell        (or 
                                                              (assq 'len  e)
                                                              (cons 'len 0))))
                                      (if debug-translate-notes (begin
                                                                  (display 'tmp-chord-off)
                                                                  (display len)
                                                                  (display e)
                                                                  (newline)))
                                      (set-cdr! len-cell (state-sustain-mode len))

                                      (chord-loop)))))

                              (if (eq? type 'tmp-tie )
                                ; tie
                                (begin
                                  (cons 
                                    (list
                                      (cons 'type 'note)
                                      (cons 'pos  position)
                                      (cons 'tie #t)
                                      (cons 'len (state-sustain-mode len)))
                                    (loop (cdr notes))))
                                ; rest
                                (begin
                                  (loop (cdr notes))))
                              ))

                           ; note
                           ((eq? type 'note )
                            (let* ((octave    state-octave)
                                   (len       state-len)
                                   (position  state-position)
                                   (velocity  state-velocity)
                                   (len
                                     (cdr  (or
                                             (assq 'tmp-len (car notes))
                                             (cons 'tmp-len state-len)
                                             )))
                                   (interval  
                                     (cdr (or (assq 'tmp-interval (car notes))
                                              (cons 'tmp-interval #f )
                                              )))
                                   (input-direction
                                     (cdr (or 
                                            (assq 'tmp-direction (car notes))
                                            (cons 'tmp-direction #f))))
                                   (direction input-direction))

                              (if debug-translate-notes (begin
                                                          (display (format "tmp-direction ~a" direction ))
                                                          (newline)))

                              ; Let's make two value-corrections in order to
                              ; process notes right; see the comments below:
                              (cond
                                ((or (not state-interval)
                                     (not interval ) ; added (Wed, 17 Oct 2018 15:07:14 +0900)
                                     (=   state-interval interval ))
                                 ; correction A
                                 ; if tmp-direction is not defined,
                                 ; apply zero as the default value.
                                 (if (not direction)
                                   (set! direction 0))

                                 ; do nothing; use these values as they are
                                 )

                                ((< state-interval interval )
                                 ; correction A
                                 ; if tmp-direction is not defined,
                                 ; apply the last value as the default value.
                                 (if (not direction)
                                   (set! direction state-direction))

                                 ; correction B ... do octave correction
                                 (if (< 0 direction )
                                   (set! direction (- direction 1))))

                                ((< interval state-interval )
                                 ; correction A
                                 ; if tmp-direction is not defined,
                                 ; apply the last value as the default value.
                                 (if (not direction)
                                   (set! direction state-direction))

                                 ; correction B ... do octave correction
                                 (if (< direction 0 )
                                   (set! direction (+ direction 1)))))
                              
                              (if debug-translate-notes (begin (display (format 
                                                                          "\
                                                                          interval: ~a
                                                                          state-interval: ~a
                                                                          input-direction: ~a
                                                                          state-direction: ~a
                                                                          direction: ~a
                                                                          "
                                                                          interval
                                                                          state-interval
                                                                          input-direction
                                                                          state-direction
                                                                          direction
                                                                          ))
                                                               (newline)
                                                               ))

                              ; calculate note value
                              (set! octave
                                (+ octave direction))

                              ; calculate note len

                              ; store the current values
                              (set! state-octave   octave)

                              ; (if (not interval ) ; just see what's happening (Wed, 17 Oct 2018 15:07:14 +0900)
                              ;   (set! state-interval interval))

                              (if interval 
                                (set! state-interval interval))

                              (if input-direction 
                                (set! state-direction input-direction))

                              ; Set the current length to the state object.
                              (set! state-len len)

                              ; Advance the current position
                              (if (not state-chord-mode)
                                (set! state-position (+ position len)))

                              (if debug-translate-notes (begin
                                                          (display (format "loop : \
                                                                           direction ~a \
                                                                           state-direction ~a \
                                                                           octave ~a \
                                                                           "
                                                                           direction
                                                                           state-direction
                                                                           octave
                                                                           ))

                                                          (newline) ))

                              ; return a link between the current note and the following object
                              (cons
                                ; Create a new note object
                                (let ((new-note 
                                        (append 
                                          (car notes)
                                          (list
                                            (cons 'pos position)
                                            (cons 'len (state-sustain-mode len))
                                            (cons 'velo velocity )
                                            (cons 'note (+ 
                                                          ; Note that the note number of C1 is 24.
                                                          (* 12 (+ octave 1))
                                                          interval))))))
                                  ; if state-chord-mode then push it to the stack
                                  (if state-chord-mode
                                    (state-chord-mode-push new-note))
                                  ; Return the current new note.
                                  new-note)

                                ; go to the next element
                                (loop (cdr notes)))
                              ))

                           ))))))

    (if debug-translate-notes (begin
                                (display 'result2)
                                (display result)
                                (newline)
                                ))

    ; delete tmp elements
    (set! result (let loop ((notes result))
                   (if (null? notes)
                     '()
                     (cons 
                       (filter-map (lambda (e)
                                     (if (regex-match (regex "^tmp")
                                                      (symbol->string (car e)))
                                       #f
                                       e))
                                   (car notes))
                       (loop (cdr notes)))
                     )))

    result))

; melody : parse simple MML commands and generate pulsar's note data.
; (melody '( do re mi fa sol ) )
; => (((type . note) (pos . 0)   (len . 1/4) (velo . 3/5) (note . 60))
;     ((type . note) (pos . 1/4) (len . 1/4) (velo . 3/5) (note . 62))
;     ((type . note) (pos . 1/2) (len . 1/4) (velo . 3/5) (note . 64))
;     ((type . note) (pos . 3/4) (len . 1/4) (velo . 3/5) (note . 65))
;     ((type . note) (pos . 1)   (len . 1/4) (velo . 3/5) (note . 67))
;     ((type . len)  (val . 5/4)))

(define (melody notes)
  (translate-notes (parse-notes (cor notes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; appending note lists
;
(define (get-len notes)
  (let ((len-note
         (find (lambda (note)
                      (eq? 'len (cdr (or (assq 'type note)
                                         (cons 'type #f)) )))
                    (reverse notes) )))    
    (if len-note
      (cdr (or (assq 'value len-note)
               (cons 'value #f)))
      #f)))

(define debug-append-notes #f)

(define (append-notes . args )
  (if debug-append-notes (begin
                           (display args)
                           (newline)
                           ))
  (let-values (((notes len)
                (let loop ((bars args)
                           (pos 0 ))
                  (if (null? bars ) 
                    (values '() pos)
                    (let ((len (get-len (car bars))))
                      (if len
                        (let-values (((notes len)
                                      (loop (cdr bars) (+ pos len ))))
                                    (values (append
                                             (mov! pos (car bars))
                                              notes)
                                            len))
                        (raise "no 'len' note was found" )))))))
              (append
               (filter (lambda(note)
                         (not (eq? 'len (cdr (or (assq 'type note )
                                            (cons 'type #f))))))
                       notes)
                (list
                 (list
                  (cons 'type 'len)
                  ; CAUTION : 'val not 'value (Tue, 23 Oct 2018 14:25:27 +0900)
                  (cons 'val len)))
                )))

#| 
 | (append-notes 
 |  (melody '( do re mi fa end ))
 |  (melody '( do re mi fa end ))
 |  (melody '( do re mi fa end ))
 |  )
 |#










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-perc (lambda (port chan note len)
                    (lambda (mark enab  pos velo . args )
                      (let ((port port)
                            (chan chan)
                            (note note)
                            (len len)
                            (args args )) 
                        (if (not (null? args)) 
                          (begin
                            (set! len (car args))
                            (set! args (cdr args))))

                        (list (cons 'type 'note  )
                              (cons 'mark  mark )
                              (cons 'enab  enab )
                              (cons 'port  port )
                              (cons 'chan  chan )
                              (cons 'pos   pos  )
                              (cons 'note  note )
                              (cons 'len   len  )
                              (cons 'velo  velo ))))))


(define noop (lambda args #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define len (lambda (value) 
  (list
    (cons 'type 'len) 
    ; CAUTION : 'val not 'value (Tue, 23 Oct 2018 14:25:27 +0900)
    (cons 'val  value ))))


; huma is deprecated.
; (define (huma key min-value max-value) 
;   (list (cons 'type    'huma) 
;         (cons 'key  key )
;         (cons 'min min-value )
;         (cons 'max max-value )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mov! (lambda (value notes)
              (map (lambda (v) 
                     (let ((e (or (assq 'pos v)
                                  (cons 'pos 0)))) 
                       (set-cdr! e (+ value (cdr e))))
                     v)
                   notes)
              
              notes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sca! (lambda (value notes)
              (map (lambda (v) 
                     (let ((e (or (assq 'pos v)
                                  (cons 'pos 0)))) 
                       (set-cdr! e (* value (cdr e))))
                     v)
                   notes)
              notes))

(define cpy (lambda args
               (if (= (length args) 0 )
                 #f
                 ; (notes . proc-list )
                 (let* ((tmp (reverse args))
                        (notes (car tmp)) ; the last element
                        (proc-list (reverse (cdr tmp ))))

                   (fold 
                     ; loop proc
                     (lambda(proc  notes)
                       (proc notes))
                     ; loop init value
                     (map 
                       (lambda (x) 
                         (if (list? x )
                           (alist-copy x)
                           x))
                       notes)
                     ; loop list
                     proc-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lookup-notes (lambda (key value notes) 
                       (find (lambda(curr-val)
                               (let ((value2 (cdr (or 
                                                    (assq key curr-val) 
                                                    (cons key #f)))))
                                 (cond
                                   ((pair? value2) 
                                    (find (lambda (fv) (eq? fv value)) value2))
                                   ((symbol? value2) 
                                    (eq? value value2))
                                   (else
                                     #f))))
                             notes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define centering-notes! (lambda ( mark pos notes )
                          (let ((note-a
                                  (lookup-notes 'mark mark notes )))

                            (if note-a 
                              (let* ((pair-pos-a (assq 'pos note-a) )
                                     (trans      (- pos 
                                                    (if pair-pos-a
                                                      (cdr pair-pos-a)
                                                      0))))

                                (if (eqv? trans 0 )
                                  notes
                                  (mov! trans notes )))
                              notes))))


(define default-id-counter 0)
(define (make-default-id)
  (set! default-id-counter (+ 1 default-id-counter))
  (string->symbol
    (string-append 
      "seq-" 
      (number->string default-id-counter))))


(define (send! p #!key (name #f) (start #f) (end #f) (once #f) )
  (if (not name)
    (set! name (make-default-id)))

  (if once 
    (set! p (list p)))

  ;p ... current pos
  ;c ... counter
  (let ((cp p)
        (c 0))
    (put-seq! name (lambda ()
                     (if (or (not (pair? cp))
                             (null? cp)
                             (and end (<= end c)))

                       ; end the loop
                       (list 
                         (n type: 'end )
                         (n type: 'len val: 5 ))

                       ; value ...  ;current value
                       (let ((value (car cp)))
                         (set! cp (cdr cp))
                         (set! c (+ 1 c ))
                         value)
                       ))
              'immediate)))


(define (make-standard-sequence p) (let ((start-pos p)
                                         (current-pos p))
                                     (lambda args
                                       (if (= 0 (length args))
                                         ; if no argument is specified, return the next bar.
                                         (if (null? current-pos) 
                                           (n
                                             (n type: 'len val: 1 )
                                             (n type: 'end ))

                                           (let ((result (car current-pos)))
                                             (set! current-pos (cdr current-pos))
                                             result))

                                         ; otherwise regard the first element of arguments as a command.
                                         ; and execute it.
                                         (let ((cmd (car args)))
                                           (cond
                                             ((eq? 'head cmd )
                                              (set! current-pos  start-pos))
                                             ((eq? 'jump cmd )
                                              (let ((i (cadr args)))
                                                (set! current-pos  (drop  start-pos i))))
                                             (else
                                               (raise (format "unknown command ~a" cmd )))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define send2! (lambda (x)
                (set! default-id-counter (+ 1 default-id-counter))
                (let ((id 
                        (string->symbol
                          (string-append 
                            "seq-" 
                            (number->string default-id-counter )))))
                  (put-seq! id
                            (lambda() 
                              (append x
                                      '(((type . end)))))
                            'immediate 
                            ))))

; Process ProgressBar
; This moved here at (Sat, 27 Jul 2019 07:34:16 +0900)

; Now displaying the current position in a progress bar is implemented 
; in Pulsar class; the following code is not necessary anymore.
; >>> (Sat, 27 Jul 2019 15:28:31 +0900)

(define default-pb-pos-setter
    (lambda (pos) 
      (set-progress-pos! pos )))

(define create-progress-bar-seq
  (lambda (seq-selector pb-pos-setter)
    (let* 
      ((timer-0 (mktimer 10000 50 (lambda args  
                                   (call-with-current-continuation
                                     (lambda (k)
                                       (with-exception-handler
                                         (lambda (x)
                                           (display "condition: ")
                                           (print-stack-trace x)
                                           (newline)
                                           (k 'exception))
                                         (lambda ()
                                           (let ((pos 
                                                   (apply
                                                     (cdr
                                                       (or (assq 'position 
                                                                 (seq-selector (list-seq)))
                                                           (cons 'not-found 
                                                                 (lambda () 0) )))
                                                     '())))
                                             (display '===)(newline)
                                             (display (list-seq) )(newline)
                                             (display pos)(newline)
                                             (pb-pos-setter pos))))))))))
      timer-0)))

(if #f
  (create-progress-bar-seq (lambda (seqlst) 
                             (cdr (or (assq 'main seqlst)
                                      (begin 
                                        ; (display-warn "PROGRESS-BAR: WARNING!!!!! COULD NOT GET 'main !!!" )
                                        ; (newline-warn)
                                        (list
                                          (cons 'position (lambda() 0.5 ))
                                          (cons 'dummy    (lambda() 0 ))))
                                      )))
                           default-pb-pos-setter))



(define make-pb-pos-setter
  (lambda (progressbar-0 ::javax.swing.JProgressBar ) 
    (lambda (pos) 
      (progressbar-0:setValue (* pos 1000))
      (progressbar-0:repaint)
      (progressbar-0:revalidate)
      (progressbar-0:repaint))))

; <<< (Sat, 27 Jul 2019 15:28:31 +0900)


; vim: filetype=scheme expandtab :
