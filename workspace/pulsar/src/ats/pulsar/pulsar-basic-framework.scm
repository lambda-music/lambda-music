;==============================================================================================
;
; Pulsar Code Generator
; See 
;    RHYTHM_PATTERNS
;    INSTRUMENTALS
;    TracksetManager
;    Trackset
;    Track
;    gui-init-proc!
;==============================================================================================

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

;==============================================================================================
; INSTRUMENTALS
;==============================================================================================
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
; RHYTHM_PATTERNS
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
(define TracksetManager 
  (lambda (self)
    (let ((trackset-list (cons 'trackset-list '())))
      (self 'define 'field 'trackset-list trackset-list )
      (self 'define 'field '*current-trackset #f )

      (self 'define 'method 'add-trackset        (lambda(self trackset) 
                                                   (begin
                                                     (set-cdr! trackset-list
                                                               (append 
                                                                 (cdr trackset-list)
                                                                 (list trackset))
                                                               ))
                                                   (gui-build!
                                                     (gui-get main-pane "TOOLBAR" "TRACKSET-SELECTION")
                                                     'index-from-last 0
                                                     (trackset 'gui))))

      (self 'define 'method 'remove-trackset     (lambda(self trackset) 
                                                   (if trackset 
                                                     ; process it only if trackset is an object; 
                                                     ; if no trackset exists on the list, this will be #f.
                                                     (begin
                                                       ; disable the trackset.
                                                       (trackset 'set-enabled #f)

                                                       ; if the passed trackset equals to the current trackset,
                                                       ; select a trackset by this algorithm and set it to the 
                                                       ; current track unless the trackset list is empty.
                                                       (if (eq? (self 'current-trackset) trackset ) 
                                                         ;then
                                                         (begin
                                                           (display '=========3)
                                                           (newline)
                                                           (if (<= (length (cdr trackset-list)) 1 )
                                                             ;then
                                                             (self 'current-trackset #f)

                                                             ;else
                                                             (let ((ptrackset (memq trackset (cdr trackset-list ) )))
                                                               (if ptrackset 
                                                                 ;then if there is no trackset on the next,
                                                                 (if (null? (cdr ptrackset ))
                                                                   ; then select the last one
                                                                   (self 'current-trackset (second (reverse (cdr trackset-list))))

                                                                   ; otherwise select the next one
                                                                   (self 'current-trackset (cadr ptrackset)))
                                                                 ;else
                                                                 (self 'current-trackset (last (cdr trackset-list))))))

                                                           ; update
                                                           (let ((t (self 'current-trackset)))
                                                             (if t
                                                               (begin
                                                                 (display '=========)
                                                                 (display ( t 'trackset-name ) )
                                                                 (newline)
                                                                 (t 'update-trackset-view)
                                                                 (self 'update-trackset-buttons t)
                                                                 )
                                                               (begin
                                                                 (self 'clear-trackset-view)
                                                                 ))))
                                                         ;else
                                                         (begin
                                                           (values)))

                                                       ; remove the trackset from the list.
                                                       (set-cdr! trackset-list
                                                                 (delete  
                                                                   trackset
                                                                   (cdr trackset-list)
                                                                   eq? ))
                                                       ; remove the trackset button from the toolbar.
                                                       (gui-build!
                                                         (gui-get main-pane "TOOLBAR" "TRACKSET-SELECTION")
                                                         'remove 
                                                         (trackset 'gui)
                                                         'revalidate))
                                                     ; else do nothing.
                                                     (begin
                                                       #f
                                                       
                                                       ))))
      (self 'define 'method 'clear-trackset-view   (lambda ( self ) 
                                                     (display 'clear-trackset-view)
                                                     (newline)
                                                     (gui-build!
                                                       (gui-get main-pane "TRACKS")
                                                       'remove-all
                                                       'revalidate)))

      (self 'define 'method 'current-trackset    (lambda (self . args )
                                                   (if (= 0 (length args))
                                                     ; read
                                                     (self '*current-trackset )

                                                     ; write
                                                     (let ((trackset (car args)))
                                                       ; note the trackset maybe #f
                                                       (if trackset 
                                                         (if (memq trackset (self 'trackset-list ))
                                                           ;then
                                                           (begin
                                                             ; disable the former current trackset
                                                             (if #f
                                                               (let ((c (self '*current-trackset) ))
                                                                 (if c
                                                                   (c 'set-enabled #f)))
                                                               (begin 
                                                                 ; don't do it anymore
                                                                 #t))
                                                             (self '*current-trackset trackset))
                                                           ;else
                                                           (begin
                                                             (display "could not find the specified trackset. ignored.")
                                                             (newline)))
                                                         ;else
                                                         (self '*current-trackset trackset))))))
      ; (self 'define 'method 'for-each-trackset    (lambda (self p)
      ;                                               (for-each p (cdr trackset-list ))))

      (self 'define 'method 'update-trackset-buttons (lambda (self selected-trackset )
                                                       (if (not selected-trackset ) 
                                                         (set! selected-trackset (self 'current-trackset )))
                                                       (for-each (lambda (e)
                                                                   (if (eq? selected-trackset e)
                                                                     ((e 'gui):setSelected #t)
                                                                     ((e 'gui):setSelected #f)))
                                                                 (cdr trackset-list ))))
      )))

(define trackset-manager (xnew TracksetManager ))

;==============================================================================================
; TRACKSET
;==============================================================================================
(define Trackset 
  (lambda ( self name )
    ; DON'T FORGET CDR BEFORE USE "track-list" 
    (let ((track-list (cons 'trackset-head '())))

      (self 'define 'field 'trackset-name          name )
      (self 'define 'field 'track-list             track-list )
      (self 'define 'field 'gui-is-button-selected (lambda (self value)
                                                     ((self 'gui ):setSelected value)))
      ; public
      (self 'define 'field 'gui (begin 
                                  (let ((b (javax.swing.JToggleButton (self 'trackset-name) )))
                                    (b:addActionListener
                                      (object (java.awt.event.ActionListener)
                                              ((actionPerformed (e ::java.awt.event.ActionEvent))
                                               ::void
                                               (if (b:isSelected)
                                                 (begin
                                                   (trackset-manager 'update-trackset-buttons self)
                                                   (trackset-manager 'current-trackset self )
                                                   (self 'update-trackset-view )))
                                               )))
                                    b)
                                  ))

      (self 'define 'method 'add-track (lambda (self track) 
                                         ; Add the passed track to the list.
                                         (set-cdr! track-list (cons track (cdr track-list)))

                                         ; Add the passed track's gui to the panel
                                         (let (( target-p (gui-get main-pane  "TRACKS")))
                                           (gui-build!
                                             target-p 
                                             'index 0
                                             (track 'gui)
                                             'invalidate
                                             'revalidate
                                             )
                                           (gui-repaint main-frame)
                                           ;(gui-revalidate main-frame)
                                           (gui-pack!)
                                           )))
      (self 'define 'method 'set-enabled-to-all-track  (lambda (self value)
                                                         (for-each (lambda (e)
                                                                     (e 'set-enabled value)
                                                                     (e 'update-track-view )) 
                                                                   (cdr track-list) )))
      ; public
      (self 'define 'method 'set-enabled  (lambda (self value)
                                            (self 'set-enabled-to-all-track value)))

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

      ; public
      (self 'define 'method 'clear-trackset       (lambda (self)
                                                    (self 'remove-all-tracks)))
      ; public
      (self 'define 'method 'update-trackset-view (lambda ( self ) 
                                                    (display 'update-trackset-view) (newline)
                                                    (apply gui-build! (append
                                                                        (list (gui-get main-pane  "TRACKS") )
                                                                        (list 'remove-all )
                                                                        (map 
                                                                          (lambda (track)(track 'gui))
                                                                          (cdr track-list))
                                                                        (list 
                                                                          (javax.swing.Box:createHorizontalGlue)
                                                                          'invalidate)
                                                                        ))
                                                      (main-pane:invalidate)
                                                      (main-pane:repaint)
                                                    (for-each 
                                                      (lambda (x) 
                                                        (x 'update-track-view))
                                                      (cdr track-list))
                                                    ))

      ; public
      (self 'define 'method 'trackset-to-source (lambda args
                                                  (prettify
                                                    (apply string-append
                                                           (append
                                                             (list "(add-all-tracks " )
                                                             (map (lambda(x)
                                                                    (string-append
                                                                      "\n"
                                                                      (pretty-print (x 'track-to-source ))))
                                                                  (if (eqv? 0 (length args) )
                                                                    (cdr track-list)
                                                                    args))
                                                             (list ") " ))))))



      )))

;==============================================================================================

;==============================================================================================
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

      (self 'define 'field 'track-id             track-id )
      (self 'define 'field 'enabled              #f )
      (self 'define 'field 'instrument           in-instrument )
      (self 'define 'field 'pns-pattern          in-pns-pattern)
      (self 'define 'field 'beat-offset          in-beat-offset )
      (self 'define 'field 'beat-count           in-beat-count )
      (self 'define 'field 'measure-count        in-measure-count)
      (self 'define 'field 'gui                  (gui-build! 
                                                   (gui-new 'group "Track" 'box javax.swing.BoxLayout:X_AXIS )

                                                   'constraint "hidden"
                                                   'name 'self-ref
                                                   (gui-new 'user-object self )

                                                   (gui-new 'label "Act" )
                                                   'name 'enabled
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

      (self 'define 'method 'hello               (lambda (self) (display 'hello)(newline) )  )

      (self 'define 'method 'track-to-source     (lambda (self) 
                                                   `(xnew Track 
                                                          ' ,( self 'read 'track-id     ) 
                                                          ' ,( self 'read 'instrument   )
                                                          ' ,( self 'read 'pns-pattern  )
                                                          \",( self 'read 'beat-offset  )\"
                                                          ' ,( self 'read 'beat-count   ) 
                                                          ' ,( self 'read 'measure-count ))))

      (self 'define 'method 'update-track-view   (lambda (self)
                                                   ; update Act label which denotes 'enabled
                                                   (let ((label (gui-get (self 'gui) 'enabled)))
                                                     (display 'label)
                                                     (display label)
                                                     (newline)
                                                     
                                                     (display (self 'enabled))
                                                     (newline)
                                                     (if label 
                                                       (label:setSelected (self 'enabled))))
                                                   
                                                   (let ((update-value (lambda(id) 
                                                                         (gui-set-selected 
                                                                           (gui-get (self 'read 'gui) id) 
                                                                           (self 'read id) 
                                                                           #t))))

                                                     
                                                     (update-value 'beat-count)
                                                     (update-value 'measure-count)
                                                     (update-value 'instrument )
                                                     (update-value 'pns-pattern)
                                                     )

                                                   (gui-set-text 
                                                     (gui-get (self 'read 'gui) 'text-beat-offset )
                                                     (self 'beat-offset))

                                                   ))

      (self 'define 'method 'set-enabled                   (lambda (self v)
                                                             (if (eqv? v (self 'enabled) )
                                                               ; do nothing
                                                               (begin
                                                                 #f)
                                                               (begin
                                                                 (self 'write 'enabled v)
                                                                 (self 'update-track-view)))))

      ; Update status of the components.
      (gui-invoke-later  (lambda () (self 'update-track-view))))))

(define main-pane #f )
(define main-frame #f )

(define create-new-counter (lambda ()
                             (let* ((new-number-counver 0 )
                                   (new-number (lambda()
                                                 (let (( v new-number-counver ))
                                                   (set! new-number-counver (+ v 1))
                                                   v)))
                                   (new-id (lambda (#!optional (prefix "") (suffix "" ))
                                             (string-append
                                               prefix
                                               (number->string (new-number))
                                               suffix))))
                               ; return the lambda "new-id"
                               new-id)))



(define new-trackset-id (create-new-counter))
(define new-track-id (let ((new-track-id-string (create-new-counter)))
                       (lambda ()
                         (string->symbol (new-track-id-string "track-")))))

; NOTE : (Sat, 20 Jul 2019 08:54:32 +0900)
; JavaScript has to create a function and execute it in order to create a
; closure. Lisp Scheme does not have to. In Scheme, you can create closures
; whenever by using 'let' statement.



(define activate-trackset (lambda (sel cmd usr src evt) 
                            (let (( target-p (gui-get main-pane  "TRACKS")))
                              (gui-remove-all target-p ))))
(define deactivate-trackset (lambda (sel cmd usr src evt) 
                              (let (( target-p (gui-get main-pane  "TRACKS")))
                                (gui-remove-all target-p ))))

(define clear-trackset (lambda (sel cmd usr src evt) 
                         (let ((current-trackset (trackset-manager 'current-trackset)))
                           (current-trackset 'clear-trackset )
                           (current-trackset 'update-trackset-view ))))

(define seq-base-00 (lambda()
                      (list (len 4/4))))

(define gui-init-proc!
  (lambda()
    (gui-clear!)
    ;(gui-frame-divider-position! 500 )
    (gui-frame-orientation!      'bottom)
    (gui-frame-width! 800)
    (gui-frame-height! 600)
    (gui-frame-divider-position! 800)
    (gui-repaint (gui-get-pane) )

    (set! main-frame  (gui-new 'frame ))
    (set! main-pane   (gui-new 'panel))

    (main-frame:setTitle "Scheme Code Generator for Music" )

    ; (main-pane:setPreferredSize (java.awt.Dimension 800 500) )
    (main-frame:setVisible #t)
    (main-frame:setSize 800 500)
    (main-frame:setDefaultCloseOperation javax.swing.WindowConstants:DISPOSE_ON_CLOSE)

    ; DON'T FORGET TO SET A LAYOUT MANAGER TO CONTENT PANE. 
    ; DON'T ASSUME ITS LAYOUT MANAGER IS ALWAYS BORDER LAYOUT.
    ; IT IS VERY DIFFICULT TO FIND WHY WHEN THE INSIDE PANELS RESIZE STRANGELY.
    ; (Wed, 17 Jul 2019 06:58:01 +0900)

    ; SET TITLE TO THE CONTENT PANE
    (if #f
      ((main-frame:getContentPane ):setBorder 
       (javax.swing.BorderFactory:createTitledBorder "CONTENT" )))

    (gui-layout! (main-frame:getContentPane )  'border )
    (gui-build! (main-frame:getContentPane )  main-pane)

    (gui-layout! main-pane  'border )

    (if #f
      (main-pane:setBorder 
        (javax.swing.BorderFactory:createTitledBorder "MAIN-PANE" )))

    (gui-build! 
      main-pane

      ; DOCUMENT ABOUT THIS
      (if #t
        (lambda (self)
          (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Sequencer" )))
        (lambda (self)
          (values)))

      'constraint java.awt.BorderLayout:PAGE_START
      'name "TOOLBAR"
      (gui-build! 
        (gui-new 'panel 'box javax.swing.BoxLayout:Y_AXIS )
        (lambda (self)
          (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Tools" )))
        'name "TRACKSET-MANAGEMENT"
        (gui-build!
          (gui-new 'panel 'box javax.swing.BoxLayout:X_AXIS )
          (lambda (self)
            (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Trackset Management" ))) 
          ; (lambda (self) (self:setPreferredSize (java.awt.Dimension 100 100)))
          (gui-new 'button "Create"     (lambda (sel cmd usr src evt) 
                                          (let ((trackset0 (xnew Trackset       (new-trackset-id "trackset" ))))
                                            (trackset-manager 'add-trackset     trackset0)
                                            (trackset-manager 'current-trackset trackset0)
                                            (trackset-manager 'update-trackset-buttons trackset0)
                                            (trackset0 'update-trackset-view )
                                            )
                                          (gui-build!
                                            main-frame
                                            'repaint
                                            'validate)))
          (gui-new 'button "Destroy"    (lambda (sel cmd usr src evt) 
                                          (trackset-manager 'remove-trackset 
                                                            (trackset-manager 'current-trackset))
                                          ))
          (gui-new 'button "Activate"   (lambda (sel cmd usr src evt)
                                          ((trackset-manager 'current-trackset) 'set-enabled #t)))
          (gui-new 'button "Deactivate" (lambda (sel cmd usr src evt)
                                          ((trackset-manager 'current-trackset) 'set-enabled #f)))
          (gui-new 'button "Clear"      clear-trackset)
          (gui-new 'button "New Track"  (lambda (sel cmd usr src evt) 
                                          ((trackset-manager 'current-trackset) 'add-track 
                                                                                (xnew Track (new-track-id)))))

          (javax.swing.Box:createHorizontalGlue )
          )

        'name "TRACKSET-SELECTION"
        (gui-build!
          (gui-new 'panel 'box javax.swing.BoxLayout:X_AXIS )
          (lambda (self)
            (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Trackset Selection" )))
          ; (lambda (self) (self:setMaximumSize (java.awt.Dimension 100 100)))
          (javax.swing.Box:createHorizontalGlue )))

      'constraint java.awt.BorderLayout:CENTER
      'name "TRACKS"
      (gui-build!
        (gui-new 'group "Tracks" 'box javax.swing.BoxLayout:Y_AXIS )
        (javax.swing.Box:createVerticalGlue))
      'invalidate
      )

    ; (gui-new 'newline)
    ; (add-track track1)
    ; (add-track track2)
    ; (add-track track3)

    (let ((trackset1 (xnew Trackset (new-trackset-id "trackset" ))))

      ; (trackset-manager 'add-trackset     trackset1)
      ; (trackset-manager 'current-trackset trackset1)
      (xnew Track (new-track-id) )
      (trackset-manager 'add-trackset trackset1)
      (trackset-manager 'current-trackset trackset1)
      (trackset1 'add-track (xnew Track (new-track-id)))
      (trackset1 'add-track (xnew Track (new-track-id)))
      (trackset1 'add-track (xnew Track (new-track-id)))
      (trackset-manager 'update-trackset-buttons trackset1)
      ; (let ((current-trackset (trackset-manager 'current-trackset))
      ;       (track1 (xnew Track (new-track-id)))
      ;       (track2 (xnew Track (new-track-id)))
      ;       (track3 (xnew Track (new-track-id))))
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
