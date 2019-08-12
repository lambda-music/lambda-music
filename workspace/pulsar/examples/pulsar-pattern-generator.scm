;==============================================================================================
;
; Pulsar Code Generator
;
; See 
;    RHYTHM_PATTERNS
;    INSTRUMENTALS
;    TracksetManager
;    Trackset
;    TrackFactoryAdapter 
;    SimpleTrack
;    create-gui
;
;==============================================================================================

(import (srfi 1))
(import (kawa pprint))

;==============================================================================================
;
;==============================================================================================

(define main-pane ::javax.swing.JPanel #!null )
(define main-frame ::javax.swing.JFrame #!null )

(define DEBUG #f)
(define TracksetManager  #f)
(define trackset-manager #f)
(define Trackset         #f)

; (display-warn "===================================" );
; (display-warn "pulsar-code-generator(1)" );
; (display-warn ((java.lang.Thread:currentThread):getName))
; (newline-warn)

;==============================================================================================
; UTIL
;==============================================================================================
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

;==============================================================================================
(set! TracksetManager 
  (lambda (self)
    (let ((trackset-list (cons 'trackset-list '()))
          (track-factory-list (cons 'track-factory-list '() ))
          )
      (self 'define 'field 'trackset-list trackset-list )
      (self 'define 'field '*current-trackset #f )

      (self 'define 'method 'add-trackset        (lambda(self trackset) 
                                                   (begin
                                                     (set-cdr! trackset-list
                                                               (append 
                                                                 (cdr trackset-list)
                                                                 (list trackset))
                                                               ))
                                                   (gui-build
                                                     (gui-get main-pane "TOOLBAR" "TRACKSET-SELECTION")
                                                     'index-from-last 1
                                                     (trackset 'gui))
                                                   trackset
                                                   ))

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
                                                           (if DEBUG (begin
                                                                       (display '=========3)
                                                                       (newline)
                                                                       ))
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
                                                                 (if DEBUG (begin
                                                                             (display '=========)
                                                                             (display ( t 'trackset-name ) )
                                                                             (newline)
                                                                             ))
                                                                 (t 'update-trackset-view)
                                                                 (self 'update-trackset-buttons t)
                                                                 )
                                                               (begin
                                                                 (self 'clear-trackset-view)
                                                                 ))))
                                                         ;else
                                                         (begin
                                                           #f))

                                                       ; remove the trackset from the list.
                                                       (set-cdr! trackset-list
                                                                 (delete  
                                                                   trackset
                                                                   (cdr trackset-list)
                                                                   eq? ))
                                                       ; remove the trackset button from the toolbar.
                                                       (gui-build
                                                         (gui-get main-pane "TOOLBAR" "TRACKSET-SELECTION")
                                                         'remove 
                                                         (trackset 'gui)
                                                         'revalidate))
                                                     ; else do nothing.
                                                     (begin
                                                       #f
                                                       
                                                       ))))
      (self 'define 'method 'clear-trackset-view   (lambda ( self ) 
                                                     (if DEBUG (begin 
                                                                 (display 'clear-trackset-view)
                                                                 (newline) ))
                                                     (gui-build
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
                                                                     ((as javax.swing.AbstractButton (e 'gui)):setSelected #t)
                                                                     ((as javax.swing.AbstractButton (e 'gui)):setSelected #f)))
                                                                 (cdr trackset-list ))))
      (self 'define 'method 'register-track-factory (lambda (self track-factory)
                                                      (set-cdr! track-factory-list
                                                                (cons track-factory track-factory-list))
                                                      
                                                      (gui-build
                                                        (gui-get main-pane "TOOLBAR" "TRACK-CONSTRUCTOR")
                                                        'index-from-last 1
                                                        (track-factory 'create-track )
                                                        )))

      (self 'define 'method 'new-trackset (lambda (self) 
                                            (let ((trackset0 (xnew Trackset       (new-trackset-id "trackset" ))))
                                              (trackset-manager 'add-trackset     trackset0)
                                              (trackset-manager 'current-trackset trackset0)
                                              (trackset-manager 'update-trackset-buttons trackset0)
                                              (trackset0 'update-trackset-view )
                                             #| (gui-build
                                                main-frame
                                                'revalidate
                                                'repaint
                                                'validate) |#
                                              trackset0)))
      )))

(set! trackset-manager (xnew TracksetManager ))
(define do-with-preserve-print-right-margin (lambda ( print-right-margin proc )
                                              (let ((old-value *print-right-margin* ))
                                                (set! *print-right-margin* print-right-margin)
                                                (let ((result (proc)))
                                                  (set! *print-right-margin* old-value)
                                                  result))))

;==============================================================================================
; TRACKSET
;==============================================================================================
(set! Trackset 
  (lambda ( self name )
    ; DON'T FORGET CDR BEFORE USE "track-list" 
    (let ((track-list (cons 'trackset-head '())))

      (self 'define 'field 'trackset-name          name )
      (self 'define 'field 'track-list             track-list )
      (self 'define 'field 'gui-is-button-selected (lambda (self value)
                                                     ((as javax.swing.AbstractButton (self 'gui )) :setSelected value)))
      ; public
      (self 'define 'field 'gui (begin 
                                  (let ((b::javax.swing.JToggleButton (javax.swing.JToggleButton (as java.lang.String (self 'trackset-name)) )))
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
      (self 'define 'method 'add-track-2 (lambda (self track) 
                                         ; Add the passed track to the list.
                                         (set-cdr! track-list (cons track (cdr track-list)))

                                         ; Add the passed track's gui to the panel
                                         (let (( target-p (gui-get main-pane  "TRACKS")))
                                           (gui-build
                                             target-p 
                                             'index-from-last 1
                                             (track 'gui)
                                             'invalidate
                                             'revalidate
                                             'repaint
                                             ))
                                         (gui-repaint main-pane)
                                         track
                                         ))

      (self 'define 'method 'add-track  (lambda (self . tracks) 
                                               (let add-track-loop ((ptrack tracks))
                                                 (if (null? ptrack)
                                                   ; end loop
                                                   #f
                                                   (begin
                                                     (let ((track (car ptrack)))
                                                       ; Add the passed track to the list.
                                                       (set-cdr! track-list (cons track (cdr track-list)))

                                                       ; Add the passed track's gui to the panel
                                                       (let (( target-p (gui-get main-pane  "TRACKS")))
                                                         (gui-build
                                                           target-p 
                                                           'index-from-last 1
                                                           (track 'gui)
                                                           'invalidate
                                                           'revalidate
                                                           'repaint
                                                           )))
                                                     (add-track-loop (cdr ptrack)))))
                                               (gui-repaint main-pane)
                                               ))

      (self 'define 'method 'set-enabled-to-all-track  (lambda (self value)
                                                         (for-each (lambda (e)
                                                                     (e 'set-enabled value)
                                                                     (e 'update-track-view )) 
                                                                   (cdr track-list) )))
      ; public
      (self 'define 'method 'set-enabled  (lambda (self value)
                                            (self 'set-enabled-to-all-track value)))

      (self 'define 'method 'remove-track (lambda (self . tracks) 
                                            (let remove-track-loop ((ptrack tracks))
                                              (if (null? ptrack ) 
                                                ;then
                                                #f
                                                ;else
                                                (let ((track (car ptrack)))
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
                                                  (gui-pack)
                                                  (gui-validate main-pane)
                                                  (remove-track-loop (cdr ptrack)))))))

      ; (self 'define 'method 'add-all-tracks (lambda (self . args )
      ;                                         (for-each (lambda (track) 
      ;                                                     (self 'add-track track))
      ;                                                   (if (eqv? 0 (length args ) )
      ;                                                     (cdr track-list)
      ;                                                     args))))

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

      ; Note that you have repaint,then revalidate. 
      ; See https://stackoverflow.com/questions/1097366/java-swing-revalidate-vs-repaint

      ; public
      (self 'define 'method 'update-trackset-view (lambda ( self ) 
                                                    ;; (display 'update-trackset-view) (newline)
                                                    (apply gui-build (append
                                                                        (list (gui-get main-pane  "TRACKS") )
                                                                        (list 'remove-all )
                                                                        (map 
                                                                          (lambda (track)(track 'gui))
                                                                          (reverse (cdr track-list)))
                                                                        (list 
                                                                          (javax.swing.Box:createHorizontalGlue)
                                                                          'invalidate
                                                                          'repaint
                                                                          'revalidate)
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
                                                  (do-with-preserve-print-right-margin 500
                                                    (lambda ()
                                                      (prettify
                                                        (apply string-append
                                                               (append
                                                                 (list "((trackset-manager 'new-trackset) 'add-track " )
                                                                 (map (lambda(x)
                                                                        (if DEBUG (begin
                                                                                    (display x)
                                                                                    (newline)
                                                                                    ))
                                                                        (string-append
                                                                          "\n"
                                                                          (pretty-print (x 'track-to-source ))))
                                                                      (if (<= (length args) 1 )
                                                                        (reverse (cdr track-list))
                                                                        ; the first element is self so go to the next element.
                                                                        (cdr args )))
                                                                 (list ") " ))))))))



      )))



;==============================================================================================
; TRACKSET
;==============================================================================================

(define TrackFactoryAdapter 
  (lambda (self track-name track-class )
    (self 'define 'field  'track-name    track-name    )
    (self 'define 'method 'track-class   track-class )
    (self 'define 'method 'create-track (lambda ( self )
                                          (gui-new 'button (cons track-name 'hello )
                                                   (lambda (sel cmd usr src evt ) 
                                                     ((trackset-manager 'current-trackset) 
                                                      'add-track (xnew track-class (new-track-id)))))))))


;==============================================================================================
; SimpleTrack
;==============================================================================================
(define SimpleTrack 
  (lambda ( self 
            track-id
            #!optional 
            (in-instrument  'inst-Kick-Long )
            (in-pns-pattern 'pns-basic-one )
            (in-velo-values "(cl 2/4 2/4 2/4 3/4 )" )
            (in-beat-offset "(+ -0/4 -0/32)" )
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
                                      (if DEBUG (begin
                                                  (display cmd)
                                                  (newline) 
                                                  ))
                                      (self 'write field-name usr )
                                      (update-inst)))))))
             (update-inst (lambda ()
                            (let ((instrument    (self 'read 'instrument ))
                                  (pns-pattern   (self 'read 'pns-pattern ))
                                  (beat-count    (self 'read 'beat-count ))
                                  (measure-count (self 'read 'measure-count ))
                                  (velo-values
                                    (eval (read (open-input-string (self 'read 'velo-values)))))
                                  (beat-offset
                                    (eval (read (open-input-string (self 'read 'beat-offset)))))
                                  (enabled       (self 'read 'enabled)))
                              (if enabled
                                (put-track (new-track track-id 
                                                      (lambda ()
                                                        (let ((notes (n-swing beat-count measure-count 
                                                                              (bind-pns (cdr (sym2val instrument inst-list )) 
                                                                                        (cdr (sym2val pns-pattern pns-list )))) ))
                                                          ; The notes object contains non-note objects, too.
                                                          ; So we have to filter the list. 
                                                          (n velo: << (lm (+ 1  (nc notes)) velo-values ) notes)
                                                          ;notes ;;; comment out now (n) duplicates notation data.
                                                                 ;;; (Thu, 08 Aug 2019 18:50:26 +0900)
                                                          )))
                                           'parallel (get-track 'main) beat-offset)
                                (remove-track (get-track track-id)))))))

      (self 'define 'field 'track-id             track-id )
      (self 'define 'field 'enabled              #f )
      (self 'define 'field 'instrument           in-instrument )
      (self 'define 'field 'pns-pattern          in-pns-pattern)
      (self 'define 'field 'velo-values          in-velo-values )
      (self 'define 'field 'beat-offset          in-beat-offset )
      (self 'define 'field 'beat-count           in-beat-count )
      (self 'define 'field 'measure-count        in-measure-count)
      (self 'define 'field 'gui                  (gui-build 
                                                   (gui-new 'group "Simple-Track" 'box javax.swing.BoxLayout:X_AXIS )

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
                                                              ; (key-name-value-list-to-name-value-list  inst-list )
                                                              (list 'selected) 
                                                              2 )
                                                            (list (lambda (sel cmd usr src evt ) 
                                                                    (if DEBUG (begin
                                                                                (display cmd)
                                                                                (newline) 
                                                                                ))
                                                                    (self 'write 'instrument usr )
                                                                    (update-inst)))))

                                                   (gui-new 'label " " )
                                                   (gui-new 'label "Pat" ); 'name 'pattern
                                                   'name 'pns-pattern

                                                   (apply gui-new 
                                                     (append
                                                       (list 'combo )
                                                       (insert-list-by-index 
                                                         (key-name-value-list-to-name-key-list pns-list )
                                                         (list 'selected) 
                                                         2 )
                                                       
                                                       (list
                                                         (lambda (sel cmd usr src evt ) 
                                                           (if DEBUG (begin
                                                                       (display cmd)
                                                                       (newline) 
                                                                       ))
                                                           (self 'write 'pns-pattern usr )
                                                           (update-inst)))))
                                                   #|
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
                                                   |#
                                                   (gui-new 'label " " )
                                                   (gui-new 'label "Beat" )
                                                   'name 'beat-count
                                                   (make-combo 'beat-count 3 )

                                                   (gui-new 'label " " )

                                                   (gui-new 'label "Bar" )
                                                   'name 'measure-count
                                                   (make-combo 'measure-count 0 )

                                                   (gui-new 'label "  " )
                                                   (gui-new 'label "VELOS" )
                                                   'name 'text-velo-values
                                                   (gui-new 'text-field  "" 32 
                                                            (lambda (sel cmd usr src evt ) 
                                                              (if DEBUG (begin
                                                                          (display cmd)
                                                                          (newline) 
                                                                          ))
                                                              (self 'write 'velo-values cmd )
                                                              (update-inst)))
                                                   (gui-new 'label "  " )
                                                   (gui-new 'label "OFFS" )
                                                   'name 'text-beat-offset
                                                   (gui-new 'text-field  "" 12 
                                                            (lambda (sel cmd usr src evt ) 
                                                              (if DEBUG (begin
                                                                          (display cmd)
                                                                          (newline) 
                                                                          ))
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

      ; public
      (self 'define 'method 'track-to-source     (lambda (self) 
                                                   (let ((proc-symbol (lambda(v)
                                                                        (string-append 
                                                                          "'" 
                                                                          (symbol->string v))))
                                                         (proc-string (lambda(v)
                                                                        (string-append 
                                                                          "\"" 
                                                                          v
                                                                          "\"" 
                                                                          )))
                                                         (proc-number (lambda(v)
                                                                        (number->string v)))
                                                         )
                                                     `(xnew SimpleTrack 
                                                            ;,(proc-symbol (self 'read 'track-id     ))
                                                             (new-track-id)
                                                            ,(proc-symbol (self 'read 'instrument   ))
                                                            ,(proc-symbol (self 'read 'pns-pattern  ))
                                                            ,(proc-string (self 'read 'velo-values  ))
                                                            ,(proc-string (self 'read 'beat-offset  ))
                                                            ,(proc-number (self 'read 'beat-count   )) 
                                                            ,(proc-number (self 'read 'measure-count))))))

      (self 'define 'method 'update-track-view   (lambda (self)
                                                   ; update Act label which denotes 'enabled
                                                   (let ((label (gui-get (self 'gui) 'enabled)))
                                                     (if DEBUG (begin
                                                                 (display 'label)
                                                                 (display label)
                                                                 (newline)

                                                                 (display (self 'enabled))
                                                                 (newline)
                                                                 ))
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
                                                     (gui-get (self 'read 'gui) 'text-velo-values )
                                                     (self 'velo-values ))
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

(define simple-track-factory (xnew TrackFactoryAdapter "Simple-Track" SimpleTrack ))




; (define main (lambda()
;                (list (len 4/4))))
; 
; (set-main (lambda()
;              (display-warn "====set-main SET-MAIN  ========\n" )
;              (newline-warn)
;              (add-tracks 'main  (lambda args
;                                 (display-warn "====set-main SET-MAIN INSIDE ========\n" )
;                                 (newline-warn)
;                                 (apply (eval 'main) args ))))))


; Note: 
; it is possible to debug inside these object.  `trackset-manager` is the root
; of those panel objects. So you can retrieve arbitrary leaf objects from the root as: 
;
; >   (symbol?
; >    ((cadr ((cadr (trackset-manager 'trackset-list)) 
; >            'track-list)) 'instrument ))
; 
; Note that 'trackset-list and 'track-list return lists.  The first element is
; a cons cell which the cdr points to the list.  I usually regard the first
; element on a list as its pointer to the list. This makes modifying lists much
; easier.
;
; + (trackset-manager
;     +trackset
;          +track
;          +track
;          +track
;     +trackset
;          +track
;          +track
;          +track
;     +trackset
;          +track
;          +track
;          +track

(define create-gui
  (lambda()
    (gui-clear)
    (gui-frame-width 800)
    (gui-frame-height 600)
    (gui-repaint (gui-get-pane) )

    (set! main-frame  (gui-new 'frame ))
    (set! main-pane   (gui-new 'panel))

    (main-frame:setTitle "Scheme Code Generator for Music" )

    ; (main-pane:setPreferredSize (java.awt.Dimension 800 500) )
    (main-frame:setVisible #t)
    (main-frame:setSize 1000 500)
    (main-frame:setLocation 800 0 )
    (main-frame:setDefaultCloseOperation javax.swing.WindowConstants:DISPOSE_ON_CLOSE)

    ; DON'T FORGET TO SET A LAYOUT MANAGER TO CONTENT PANE. 
    ; DON'T ASSUME ITS LAYOUT MANAGER IS ALWAYS BORDER LAYOUT.
    ; IT IS VERY DIFFICULT TO FIND WHY WHEN THE INSIDE PANELS RESIZE STRANGELY.
    ; (Wed, 17 Jul 2019 06:58:01 +0900)

    ; SET TITLE TO THE CONTENT PANE
    (if #f
      ((main-frame:getContentPane ):setBorder 
       (javax.swing.BorderFactory:createTitledBorder "CONTENT" )))

    (gui-layout (main-frame:getContentPane )  'border )
    (gui-build (main-frame:getContentPane )  main-pane)

    (gui-layout main-pane  'border )

    (if #f
      (main-pane:setBorder 
        (javax.swing.BorderFactory:createTitledBorder "MAIN-PANE" )))

    (gui-build 
      main-pane

      ; DOCUMENT ABOUT THIS
      (if #t
        (lambda (self ::javax.swing.JPanel )
          (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Sequencer" )))
        (lambda (self)
          #f))

      'constraint java.awt.BorderLayout:PAGE_START
      'name "TOOLBAR"
      (gui-build 
        (gui-new 'panel 'box javax.swing.BoxLayout:Y_AXIS )
        (lambda (self ::javax.swing.JPanel)
          (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Tools" )))
        'name "TRACKSET-MANAGEMENT"
        (gui-build
          (gui-new 'panel 'box javax.swing.BoxLayout:X_AXIS )
          (lambda (self ::javax.swing.JPanel)
            (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Trackset Management" ))) 
          ; (lambda (self) (self:setPreferredSize (java.awt.Dimension 100 100)))
          (gui-new 'button "Create"     (lambda (sel cmd usr src evt) 
                                          (trackset-manager 'new-trackset)))



          (gui-new 'button "Activate"   (lambda (sel cmd usr src evt)
                                          ((trackset-manager 'current-trackset) 'set-enabled #t)))

          (gui-new 'button "Deactivate" (lambda (sel cmd usr src evt)
                                          ((trackset-manager 'current-trackset) 'set-enabled #f)))
          (gui-new 'button "Source"     (lambda (sel cmd usr src evt)
                                          (gui-insert-text
                                            (string-append
                                              ((trackset-manager 'current-trackset) 'trackset-to-source )
                                              "\n")
                                          )))




          (javax.swing.Box:createHorizontalGlue )
          (gui-new 'button "Clear"      (lambda (sel cmd usr src evt) 
                                          (let ((current-trackset (trackset-manager 'current-trackset)))
                                            (current-trackset 'clear-trackset )
                                            (current-trackset 'update-trackset-view ))))
          (gui-new 'button "Destroy"    (lambda (sel cmd usr src evt) 
                                          (trackset-manager 'remove-trackset 
                                                            (trackset-manager 'current-trackset))))
          )
        'name "TRACK-CONSTRUCTOR"
        (gui-build
          (gui-new 'panel 'box javax.swing.BoxLayout:X_AXIS )
          (lambda (self ::javax.swing.JPanel)
            (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Track Construction" )))
          ; (lambda (self) (self:setMaximumSize (java.awt.Dimension 100 100)))
          (gui-new 'button "Simple-Track"  (lambda (sel cmd usr src evt) 
                                             ((trackset-manager 'current-trackset) 'add-track 
                                                                                   (xnew SimpleTrack (new-track-id)))))
          (javax.swing.Box:createHorizontalGlue ))

        'name "TRACKSET-SELECTION"
        (gui-build
          (gui-new 'panel 'box javax.swing.BoxLayout:X_AXIS )
          (lambda (self ::javax.swing.JPanel)
            (self:setBorder (javax.swing.BorderFactory:createTitledBorder "Trackset Selection" )))
          ; (lambda (self) (self:setMaximumSize (java.awt.Dimension 100 100)))
          (javax.swing.Box:createHorizontalGlue ))

        )

        

      'constraint java.awt.BorderLayout:CENTER
      'name "TRACKS"
      (gui-build
        (gui-new 'group "Tracks" 'box javax.swing.BoxLayout:Y_AXIS )
        (javax.swing.Box:createVerticalGlue))
      'invalidate
      )

    ; (gui-new 'newline)
    ; (add-track track1)
    ; (add-track track2)
    ; (add-track track3)

    (if #f
      (let ((trackset1 (xnew Trackset (new-trackset-id "trackset" ))))
        (trackset-manager 'add-trackset     trackset1)
        (trackset-manager 'current-trackset trackset1)
        (trackset1 'add-track 
                   (xnew SimpleTrack (new-track-id))
                   (xnew SimpleTrack (new-track-id)))

        (trackset-manager 'update-trackset-buttons trackset1)
        (trackset1 'update-trackset-view    )

        ; (let ((current-trackset (trackset-manager 'current-trackset))
        ;       (track1 (xnew SimpleTrack (new-track-id)))
        ;       (track2 (xnew SimpleTrack (new-track-id)))
        ;       (track3 (xnew SimpleTrack (new-track-id))))
        ;   ; (current-trackset 'add-track track1)
        ;   ; (current-trackset 'add-track track2)
        ;   ; (current-trackset 'add-track track3))
        ;   #f
        ;   )
        ))

    

    (gui-pack)
    (trackset-manager 'register-track-factory simple-track-factory )
    ))

(newline)
; (create-gui)

; (display-warn "===================================" );
; (display-warn "pulsar-code-generator(2)" );
; (display-warn ((java.lang.Thread:currentThread):getName))
; (newline-warn)

; vim: filetype=scheme expandtab :
