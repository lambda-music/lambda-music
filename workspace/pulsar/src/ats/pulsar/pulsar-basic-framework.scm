(import (srfi 1))
(import (kawa pprint))

; n= a number of swing sequences ( c8 r8 c8 c8 ) which length usually equals to a halftone.
; ex (n-swing 2 1) generates the orthodox swing pattern which consists 4 quarter tones.
(define n-swing 
  (lambda(n b ns-proc) 
    (cons 
      (len b)
      (sca! b
            (apply append 
                   (map 
                     (lambda (x)
                       (mov! (/ x n)
                             (sca! (/ 1 n)
                                   (ns-proc x n))))
                     (iota n )))))))

; ====================================================================================

; bind-pns
; This procedure binds a pns(plain n-swing) procedure with an instrument
; procedure and returns ns-proc.
(define bind-pns (lambda (inst pns-proc) 
                       (lambda (x n)
                         (pns-proc inst x n))))

(define make-pb-pos-setter
  (lambda (progressbar-0) 
    (lambda (pos) 
      (progressbar-0:setValue (* pos 1000))
      (progressbar-0:repaint)
      (progressbar-0:revalidate)
      (progressbar-0:repaint))))

(define default-pb-pos-setter
    (lambda (pos) 
      (set-progress-pos! pos )))

(define create-progress-bar-seq
  (lambda (seq-selector pb-pos-setter)
    (let* 
      ((timer-0 (gui-new 'timer 50 (lambda args  
                                 (call-with-current-continuation
                                   (lambda (k)
                                     (with-exception-handler
                                       (lambda (x)
                                         (display "condition: ")
                                         (write x)
                                         (newline)
                                         (k 'exception))
                                       (lambda ()
                                         (let ((pos 
                                                 (apply
                                                   (cdr
                                                     (assq 'position 
                                                           (seq-selector (list-seq))))
                                                   '())))
                                           ; (display pos)(newline)
                                           (pb-pos-setter pos))))))))))
      timer-0)))

(define key-name-value-list-to-name-value-list 
  (lambda ( key-name-value-list )
    (map 
      (lambda (e) (cons (cadr e) (cddr e))) 
      key-name-value-list )))

(define key-name-value-list-to-name-key-list 
  (lambda ( key-name-value-list )
    (map 
      (lambda (e) (cons (cadr e) (car e))) 
      key-name-value-list )))

(define insert-list-by-index 
  (lambda (dst-list src-list idx)
    (let-values (((head tail) (split-at dst-list idx )))
                (append head src-list tail))))
  
(define name-value-list-select-by-value 
  (lambda ( name-value-list value )
    #f
    ))

(define inst-list (list (cons 'inst-cowb   (cons "Cowbell"    (make-perc 0 0 D7  1/8 )))
                        (cons 'inst-rid1   (cons "Ride"       (make-perc 0 0 Db5 1/8 )))
                        (cons 'inst-drys   (cons "Dry Snare"  (make-perc 0 0 G3  1/8 )))
                        (cons 'inst-snar   (cons "Snare"      (make-perc 0 0 A3  1/8 )))
                        (cons 'inst-snar2  (cons "Snare2"     (make-perc 0 0 B3  1/8 )))
                        (cons 'inst-kikl   (cons "Kick Long"  (make-perc 0 0 E4  1/8 )))
                        (cons 'inst-kik    (cons "Kick"       (make-perc 0 0 F4  1/8 )))
                        (cons 'inst-hhp1   (cons "HHPedal"    (make-perc 0 0 G2  1/8 )))
                        ))

(define inst-cowb  (cddr (assq 'inst-cowb   inst-list )))
(define inst-hhp1  (cddr (assq 'inst-hhp1   inst-list )))
(define inst-drys  (cddr (assq 'inst-drys   inst-list )))
(define inst-rid1  (cddr (assq 'inst-rid1   inst-list )))

(define inst-snar  (cddr (assq 'inst-snar   inst-list )))
(define inst-snar2 (cddr (assq 'inst-snar2  inst-list )))
(define inst-kikl  (cddr (assq 'inst-kikl   inst-list )))
(define inst-kik   (cddr (assq 'inst-kik    inst-list )))

(define cnt-a (make-perc 1 0 (+ B3  0)  1/16 ))
(define cnt-b (make-perc 1 0 (+ Bb3 1 )  1/16 ))

(define cnt0  (make-perc 1 0 (+ C4  0)  1/4 ))
(define cnt1  (make-perc 1 0 (+ C4  1)  1/4 ))
(define cnt2  (make-perc 1 0 (+ C4  2)  1/4 ))
(define cnt3  (make-perc 1 0 (+ C4  3)  1/4 ))

(define count-voices 
  (map 
    (lambda (x)
      (make-perc 1 0 (+ C4  x)  1/4 ))
    (iota 20)))

; ===========================================================================================

(define pns-two-four
  (lambda (inst0 x n) 
    (list
      (inst0 'A           #t (/ 2 4) (+ 0.5  (rnd -0.0  0.1 )))))) 

(define pns-basic-one 
  (lambda (inst0 x n) 
    (list
      (inst0 'A (luck 1.00 ) 0 (+ 0.3  (rnd -0.0  0.1 )))
      ))) 

(define pns-basic-4-swing 
  (lambda (inst0 x n) 
    (list
      (inst0 'A (luck 1.00 ) (/ 0 4) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 0.00 ) (/ 1 4) (+ 0.5  (rnd -0.0  0.1 )))
      (inst0 'A (luck 1.00 ) (/ 2 4) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 1.00 ) (/ 3 4) (+ 0.5  (rnd -0.0  0.3 )))
      ))) 

(define pns-basic-6-swing 
  (lambda (inst0 x n) 
    (list
      (inst0 'A (luck 1.00 ) (/ 0 6) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 0.00 ) (/ 2 6) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 1.00 ) (/ 3 6) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 1.00 ) (/ 5 6) (+ 0.5  (rnd -0.0  0.3 )))
      ))) 

(define pns-basic-5-swing 
  (lambda (inst0 x n) 
    (list
      (inst0 'A (luck 1.00 ) (/ 0 5) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 0.00 ) (/ 1 5) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 1.00 ) (/ 2 5) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 0.00 ) (/ 3 5) (+ 0.3  (rnd -0.0  0.1 )))
      (inst0 'A (luck 1.00 ) (/ 4 5) (+ 0.5  (rnd -0.0  0.3 )))))) 

(define pns-ntime  
  (lambda (inst0 x n) 
    (list
      (inst0 'A (luck 1.00 ) (/ 0 4) (+ 0.3  (rnd -0.0  0.1 )))))) 

(define pns-counting
  (lambda (dummy-inst x n)
    (list
        (let ((cnt (list-ref count-voices x)))
          (cnt '(A) #t 0 (rnd 0.5 0.7 ))))))

(define create-count-measure (lambda (nm)
                               (append
                                 (map
                                   (lambda (n)
                                     (let ((cnt (list-ref count-voices n)))
                                       (cnt '(A) #t (/ n nm ) (rnd 0.5 0.7 ))))
                                   (iota nm))
                                 (list (len 1)))))

;==============================================================================================
(define TracksetManager (lambda (self)
                          (let ((trackset-list (cons 'trackset-list '())))
                            (self 'define 'field 'trackset-list trackset-list )
                            (self 'define 'field '*current-trackset #f )

                            (self 'define 'method 'add-trackset (lambda(self trackset) 
                                                                  (set-cdr! trackset-list
                                                                            (append 
                                                                              (cdr trackset-list)
                                                                              (list trackset))
                                                                            )))
                            (self 'define 'method 'remove-trackset (lambda(self trackset) 
                                                                     (set-cdr! trackset-list
                                                                               (delete  
                                                                                 trackset
                                                                                 (cdr trackset-list)
                                                                                 eq? ))))
                            (self 'define 'method 'current-trackset (lambda (self . args )
                                                                      (if (= 0 (length args))
                                                                        ; read
                                                                        (self '*current-trackset )

                                                                        ; write
                                                                        (let ((trackset (car args)))
                                                                          (if (memq trackset (self 'trackset-list ))
                                                                            ;then
                                                                            (begin
                                                                              (let ((c (self '*current-trackset) ))
                                                                                (if c
                                                                                  (c 'set-enabled #f)))
                                                                              (self '*current-trackset trackset))

                                                                            ;else
                                                                            (begin
                                                                              (display "could not find the specified trackset. ignored.")
                                                                              (newline)))))))
                            )))

(define trackset-manager (xnew TracksetManager ))


;==============================================================================================
; ==== TRACKSET =====
(define Trackset 
  (lambda ( self id )
    (let ((track-list (cons id '())))

      (self 'define 'field 'track-list track-list )
      (self 'define 'method 'set-current-track-list (lambda(self)
                                                      (set-track-list (self 'track-list ))
                                                      (update-track-list)))

      (self 'define 'field 'gui (begin 
                                  (gui-new 'button "NAME" 
                                           (lambda (sel cmd usr src evt)
                                             (self 'set-current-track-list )))))

      (self 'define 'method 'add-track (lambda (self track) 
                                         ; Add the passed track to the list.
                                         (set-cdr! track-list (cons track (cdr track-list)))

                                         ; Add the passed track's gui to the panel
                                         (let (( target-p (gui-get main-pane  "SEQUENCES")))
                                           (gui-build!
                                             target-p 
                                             'index 0
                                             (track 'gui)
                                             'invalidate
                                             )
                                           (gui-repaint main-frame)
                                           (gui-revalidate main-frame)
                                           (gui-pack!)
                                           )))
      (self 'define 'method 'set-enabled-to-all-track  (lambda (self value)
                                                         (for-each (lambda (e)
                                                                     (e 'set-enabled value)) 
                                                                   track-list )))
      (self 'define 'method 'set-enabled  (lambda (self value)
                                            (self 'set-enabled-to-all-track)
                                            ))

      (self 'define 'method 'remove-track (lambda (self track) 
                                            ; disable the track
                                            (track 'set-enabled #f)
                                            ; remove the passed track from the list.
                                            (set-cdr! track-list (delete track (cdr track-list) eq?))

                                            ; Remove the passed track's gui to the panel
                                            (let* ((gui (track 'gui))
                                                   (parent (gui-parent gui)))
                                              (if parent
                                                (gui-remove-by-ref (gui-parent gui ) gui)))

                                            (gui-repaint main-frame)
                                            (gui-pack!)
                                            (gui-validate main-pane)
                                            ))

      (self 'define 'method 'add-all-tracks (lambda (self . args )
                                              (for-each (lambda (track) 
                                                          (self 'add-track track))
                                                        (if (eqv? 0 (length args ) )
                                                          (cdr track-list)
                                                          args))))

      (self 'define 'method 'remove-all-tracks (lambda (self . args)
                                                 (for-each (lambda (track)
                                                             (self 'remove-track track))
                                                           (if (eqv? 0 (length args ) )
                                                             (cdr track-list)
                                                             args))
                                                 (set-cdr! track-list '())))

      (self 'define 'method 'update-gui (lambda ( self ) 
                                          (for-each 
                                            (lambda (x) 
                                              (x 'update-gui))
                                            track-list)
                                          (let ((target-pane (gui-get main-pane  "SEQUENCES")))
                                            (gui-remove-all target-pane)
                                            (apply gui-build!
                                                   (append
                                                     (list target-pane)
                                                     (map 
                                                       (lambda (track)(track 'gui))
                                                       (cdr track-list))
                                                     (list 
                                                       (javax.swing.Box:createHorizontalGlue)
                                                       'invalidate)
                                                     ))


                                            )))

      (self 'define 'method 'all-tracks-to-lisp (lambda args
                                                  (prettify
                                                    (apply string-append
                                                           (append
                                                             (list "(add-all-tracks " )
                                                             (map (lambda(x)
                                                                    (string-append
                                                                      "\n"
                                                                      (pretty-print (x 'to-lisp))))
                                                                  (if (eqv? 0 (length args) )
                                                                    (cdr track-list)
                                                                    args))
                                                             (list ") " ))))))



      )))

; (define current-trackset (xnew Trackset 'trackset-01 ))

;==============================================================================================




(define seq-base-00 (lambda()
                      (list (len 4/4))))

(define Track 
  (lambda ( self 
            track-id
            #!optional 
            (in-instrument  'inst-kikl )
            (in-pns-pattern 'pns-basic-one )
            (in-beat-offset "(+ 0/4 0 )" )
            (in-beat-count 4 )
            (in-measure-count 1))
    (letrec ((make-combo 
               (lambda (field-name index) 
                 (apply gui-new (append 
                                  ; list No.0
                                  (list 'combo )

                                  ; list No.1
                                  (let-values ((( lst0 lst1 )
                                                (split-at 
                                                  (map cons 
                                                       (map number->string (iota 21 1 1))
                                                       (iota 21 1 1))

                                                  (+ index 1) ; - add one in order to 
                                                  ;   insert after the element
                                                  )))
                                              (append lst0 (list 'selected) lst1))
                                  ; list No.2
                                  (list 
                                    (lambda (sel cmd usr src evt ) 
                                      (display cmd)
                                      (newline) 
                                      (self 'write field-name usr )
                                      (update-inst)))))))
             (update-inst (lambda ()
                            (let ((instrument    (self 'read 'instrument ))
                                  (pns-pattern   (self 'read 'pns-pattern ))
                                  (beat-count    (self 'read 'beat-count ))
                                  (measure-count (self 'read 'measure-count ))
                                  (beat-offset   
                                    (eval (read (open-input-string (self 'read 'beat-offset)))))
                                  (enabled       (self 'read 'enabled)))
                              (if enabled
                                (put-seq! track-id 
                                          (lambda ()
                                            (n-swing beat-count measure-count (bind-pns (eval instrument) (eval pns-pattern) )))
                                          'parallel 'seq-base beat-offset)
                                (remove-seq! track-id ))))))
      (self 'define 'field 'track-id       track-id )
      (self 'define 'field 'enabled        #f )
      (self 'define 'field 'instrument     in-instrument )
      (self 'define 'field 'pns-pattern    in-pns-pattern)
      (self 'define 'field 'beat-offset    in-beat-offset )
      (self 'define 'field 'beat-count     in-beat-count )
      (self 'define 'field 'measure-count  in-measure-count)
      (self 'define 'field 'gui (gui-build! 
                                  (gui-new 'group "Track" 'box javax.swing.BoxLayout:X_AXIS )

                                  'constraint "hidden"
                                  'name 'self-ref
                                  (gui-new 'user-object self )

                                  (gui-new 'label "Act" )
                                  (gui-new 'check (lambda (sel cmd usr src evt )
                                                    (self 'write 'enabled sel)
                                                    (update-inst)
                                                    ))
                                  (gui-new 'label "Inst" )

                                  'name 'instrument
                                  (apply gui-new 
                                         (append
                                           (list 'combo )
                                           (insert-list-by-index 
                                             (key-name-value-list-to-name-key-list inst-list )
                                             (list 'selected) 
                                             2 )
                                           (list (lambda (sel cmd usr src evt ) 
                                                   (display cmd)
                                                   (newline) 
                                                   (self 'write 'instrument usr )
                                                   (update-inst)))))

                                  (gui-new 'label " " )
                                  (gui-new 'label "Pat" ); 'name 'pattern
                                  'name 'pns-pattern
                                  (gui-new 'combo 
                                           (cons "1 Beat"   'pns-basic-one )
                                           'selected
                                           (cons "2-4 Beat" 'pns-two-four )
                                           (cons "4 Swing"  'pns-basic-4-swing )
                                           (cons "5 Swing"  'pns-basic-5-swing )
                                           (cons "6 Swing"  'pns-basic-6-swing )
                                           (cons "Counting" 'pns-counting )
                                           (lambda (sel cmd usr src evt ) 
                                             (display cmd)
                                             (newline) 
                                             (self 'write 'pns-pattern usr )
                                             (update-inst)))
                                  (gui-new 'label " " )
                                  (gui-new 'label "Beat" )
                                  'name 'beat-count
                                  (make-combo 'beat-count 3 )

                                  (gui-new 'label " " )

                                  (gui-new 'label "Bar" )
                                  'name 'measure-count
                                  (make-combo 'measure-count 0 )
                                  (gui-new 'label "  " )
                                  (gui-new 'label "OFFS" )
                                  'name 'text-beat-offset
                                  (gui-new 'text-field  "" 16
                                           (lambda (sel cmd usr src evt ) 
                                             (display cmd)
                                             (newline) 
                                             (self 'write 'beat-offset cmd )
                                             (update-inst)))
                                  (gui-new 'label "  " )
                                  (gui-new 'button (cons "X" 'hello )
                                           (lambda (sel cmd usr src evt ) 
                                             #| (let ((gui (self 'gui)))
                                               (gui-remove-by-ref (gui-parent gui ) gui)) |#
                                             (let ((current-trackset (trackset-manager 'current-trackset)))
                                               (current-trackset 'remove-track self))
                                             (gui-repaint (self 'gui))))
                                  (gui-new 'label " " )
                                  (lambda (x) 
                                    (x:setMaximumSize (java.awt.Dimension 10000 50)))
                                  ))
      (self 'define 'method 'hello (lambda (self) (display 'hello)(newline) )  )
      (self 'define 'method 'to-lisp 
            (lambda (self) 
              `(xnew Track 
                      ' ,( self 'read 'track-id     ) 
                      ' ,( self 'read 'instrument   )
                      ' ,( self 'read 'pns-pattern  )
                      \",( self 'read 'beat-offset  )\"
                      ' ,( self 'read 'beat-count   ) 
                      ' ,( self 'read 'measure-count ))))
      (self 'define 'method 'update-value
            (lambda (self)
              (let ((update-value (lambda(id) 
                                    (gui-set-selected 
                                      (gui-get (self 'read 'gui) id) 
                                      (self 'read id) 
                                      #t)
                                    )))
                (update-value 'beat-count)
                (update-value 'measure-count)
                (update-value 'instrument )
                (update-value 'pns-pattern)
                )

              (gui-set-text 
                (gui-get (self 'read 'gui) 'text-beat-offset )
                (self 'beat-offset))
              
              ))
      ; (self 'update-value)
      (self 'define 'method 'set-enabled (lambda (self v)
                                           (self 'write 'enabled v)
                                           (self 'update-value))))))

(define main-pane #f )
(define main-frame #f )



; (define track-list (cons 'ptr-track-list '() ))
;
; (define add-track (lambda (track) 
;                    ; Add the passed track to the list.
;                    (set-cdr! track-list (cons track (cdr track-list)))
; 
;                    ; Add the passed track's gui to the panel
;                    (let (( target-p (gui-get main-pane  "SEQUENCES")))
;                      (gui-build!
;                        target-p 
;                        'index 0
;                        (track 'gui)
;                        'invalidate
;                        )
;                      (gui-repaint main-frame)
;                      (gui-revalidate main-frame)
;                      (gui-pack!)
;                      )))
; 
; (define remove-track (lambda (track) 
;                       ; disable the track
;                       (track 'set-enabled #f)
;                       ; remove the passed track from the list.
;                       (set-cdr! track-list (delete track (cdr track-list) eq?))
; 
;                       ; Remove the passed track's gui to the panel
;                       (let ((gui (track 'gui)))
;                         (gui-remove-by-ref (gui-parent gui ) gui))
;                       (gui-repaint main-frame)
;                       (gui-pack!)
;                       (gui-validate main-pane)
;                       ))
; 
; (define add-all-tracks (lambda args
;                         (for-each add-track 
;                                   (if (eqv? 0 (length args ) )
;                                     (cdr track-list)
;                                     args))))
; 
; (define remove-all-tracks (lambda args
;                            (for-each remove-track
;                                      (if (eqv? 0 (length args ) )
;                                        (cdr track-list)
;                                        args))
;                            (set-cdr! track-list '())))
; 
; 
; (define update-track-list (lambda () 
;                            (let ((target-pane (gui-get main-pane  "SEQUENCES")))
;                              (gui-remove-all target-pane)
;                              (apply gui-build!
;                                     (append
;                                       (list target-pane)
;                                       (map 
;                                         (lambda (track)(track 'gui))
;                                         (cdr track-list))
;                                       (list 
;                                         (javax.swing.Box:createHorizontalGlue)
;                                         'invalidate)
;                                       )))))
; 
; (define set-track-list (lambda (new-track-list)
;                          ; track-list (global variable)
;                          (set-cdr! track-list new-track-list )))
; 
; (define all-tracks-to-lisp (lambda args
;                             (prettify
;                               (apply string-append
;                                      (append
;                                        (list "(add-all-tracks " )
;                                        (map (lambda(x)
;                                               (string-append
;                                                 "\n"
;                                                 (pretty-print (x 'to-lisp))))
;                                             (if (eqv? 0 (length args) )
;                                               (cdr track-list)
;                                               args))
;                                        (list ") " ))))))
; 
; 
; 



; (define symbol->string-rec (e) 
;   (map (lambda (x)
;          (if (pair? x)
;            (symbol->string-rec x)
;            (symbol->string x)))
;        e))

(define create-button  (lambda (sel cmd usr src evt) 
                         (let ((current-trackset (trackset-manager 'current-trackset)))
                           (current-trackset 'add-track 
                                             (xnew Track 'hello))
                           )
                         ))

(define create-trackset (lambda (sel cmd usr src evt) 
                          (let (( target-p (gui-get main-pane  "SEQUENCES")))
                            (gui-remove-all target-p )
                            )))
(define destroy-trackset (lambda (sel cmd usr src evt) 
                           (let (( target-p (gui-get main-pane  "SEQUENCES")))
                             (gui-remove-all target-p ))))
(define activate-trackset (lambda (sel cmd usr src evt) 
                            (let (( target-p (gui-get main-pane  "SEQUENCES")))
                              (gui-remove-all target-p ))))
(define deactivate-trackset (lambda (sel cmd usr src evt) 
                              (let (( target-p (gui-get main-pane  "SEQUENCES")))
                                (gui-remove-all target-p ))))

(define clear-trackset (lambda (sel cmd usr src evt) 
                         (let ((current-trackset (trackset-manager 'current-trackset)))
                           (current-trackset 'remove-all-tracks )
                           (current-trackset 'update-gui ))))

(define gui-init-proc!
  (lambda()
    (gui-clear!)
    ;(gui-frame-divider-position! 500 )
    (gui-frame-orientation!      'bottom)
    (gui-frame-width! 800)
    (gui-frame-height! 600)
    (gui-frame-divider-position! 800)
    (gui-repaint (gui-get-pane) )

    (set! main-frame  (gui-new 'frame))
    (set! main-pane   (gui-new 'panel))

    ; (main-pane:setPreferredSize (java.awt.Dimension 800 500) )
    (main-frame:setVisible #t)
    (main-frame:setSize 800 500)
    (main-frame:setDefaultCloseOperation javax.swing.WindowConstants:DISPOSE_ON_CLOSE)

    ; DON'T FORGET TO SET A LAYOUT MANAGER TO CONTENT PANE. 
    ; DON'T ASSUME ITS LAYOUT MANAGER IS ALWAYS BORDER LAYOUT.
    ; IT IS VERY DIFFICULT TO FIND WHY WHEN THE INSIDE PANELS RESIZE STRANGELY.
    ; (Wed, 17 Jul 2019 06:58:01 +0900)

    ((main-frame:getContentPane ):setBorder 
     (javax.swing.BorderFactory:createTitledBorder "CONTENT" ))
    (gui-layout! (main-frame:getContentPane )  'border )
    (gui-build! (main-frame:getContentPane )  main-pane)

    (gui-layout! main-pane  'border )
    (gui-build! 
      main-pane
      'name "TOOLS"
      (lambda (self)
        (self:setBorder (javax.swing.BorderFactory:createTitledBorder "ROOT" )))

      'constraint java.awt.BorderLayout:PAGE_START
      (gui-build! 
        (gui-new 'group "TOOLS" 'box javax.swing.BoxLayout:Y_AXIS)

        'name "TOOL-BUTTONS"
        (gui-build! 
          (gui-new 'panel 'box javax.swing.BoxLayout:Y_AXIS )
          (gui-build!
            (gui-new 'panel 'box javax.swing.BoxLayout:X_AXIS )
            ; (lambda (self) (self:setPreferredSize (java.awt.Dimension 100 100)))
            (gui-new 'button "Create"     create-trackset)
            (gui-new 'button "Destroy"    destroy-trackset)
            (gui-new 'button "Activate"   activate-trackset)
            (gui-new 'button "Deactivate" deactivate-trackset)
            (gui-new 'button "Clear"      clear-trackset)

            (javax.swing.Box:createHorizontalGlue )
            )

          'name "SETS"
          (gui-build!
            (gui-new 'panel 'box javax.swing.BoxLayout:X_AXIS )
            ; (lambda (self) (self:setMaximumSize (java.awt.Dimension 100 100)))
            (gui-new 'button "A"   create-button)
            (gui-new 'button "B"   create-button)
            (gui-new 'button "C"   create-button)
            (gui-new 'button "D"   create-button)
            (gui-new 'button "E"   create-button)
            (javax.swing.Box:createHorizontalGlue ))))

      'constraint java.awt.BorderLayout:CENTER
      'name "SEQUENCES"
      (gui-build!
        (gui-new 'group "Sequences" 'box javax.swing.BoxLayout:Y_AXIS )
        (javax.swing.Box:createVerticalGlue))


      'validate
      )

    ; (gui-new 'newline)
    ; (add-track track1)
    ; (add-track track2)
    ; (add-track track3)

    (let ((trackset1 (xnew Trackset 'trackset1)))
      ; (trackset-manager 'add-trackset     trackset1)
      ; (trackset-manager 'current-trackset trackset1)
      (xnew Track 'track1)
      (trackset-manager 'add-trackset trackset1)
      (trackset-manager 'current-trackset trackset1)
      ; (let ((current-trackset (trackset-manager 'current-trackset))
      ;       (track1 (xnew Track 'track1))
      ;       (track2 (xnew Track 'track2))
      ;       (track3 (xnew Track 'track3)))
      ;   ; (current-trackset 'add-track track1)
      ;   ; (current-trackset 'add-track track2)
      ;   ; (current-trackset 'add-track track3))
      ;   (values)
      ;   )
      )

    (create-progress-bar-seq (lambda (seqlst) 
                               (cdr (assq 'seq-base seqlst)))
                             default-pb-pos-setter)

    (gui-pack!)))

(newline)


; INIT
(define (open-proc!) 
  (close!)
  (open!     "pulsar" )
  (output!   "out-h2" "out-counter" "out-fluidsynth" )
  (input!    "MIDI Input0"  "MIDI Input1"  )
  (set-tempo! 120)

  (connect!  "pulsar:out-h2"         "hydrogen-midi:RX" )
  (connect!  "pulsar:out-counter"    "qsynth-counter:midi" )
  (connect!  "pulsar:out-fluidsynth" "qsynth-fluidsynth:midi" ))

(define (init-proc!) 
  (clear!)
  (set-main! (lambda()
               (put-seq! 'seq-base       (new-delegator seq-base-00))))
  (set-related-files! '( "./pulsar-ex00.scm"
                         "./test01.scm" 
                         "./test02.scm" 
                         "./test03.scm" ))
  (rewind!))

(if (not (open?))
  (begin 
    (open-proc!)
    (init-proc!)
    (gui-init-proc!)))


; vim: filetype=scheme expandtab :
