#|
  '(
    (date (Tue, 09 Oct 2018 00:58:59 +0900))
    (todo
     (0. Adding jumping to the corresponding parenthesis.)
     (1. Adding a code completion window.
         (Implementing Multiple modes))
     (2. Adding syntactic parentheses selection.
         (let ((A the character where the caret is on ))
           ((when A is an alphabet character)
            (select the word))
           ((when A is a parenthesis character )
            (select the pair of the parentheses ))))))))

|#

(define get-variables (lambda (this-object)
                    ;(display this-object)
                    ;(newline)
                    ;(display this-object:class)
                    ;(newline)
                    (let loop ((i 0)
                               ( methods (java.lang.Class:getMethods this-object:class) ))
                      (if (and (< i methods:length ) (not (eq? (methods i) #!null )))     
                        (let ((name (invoke (methods i) 'getName  )))
                          (display name)
                          (newline)
                          (cons name (loop (+ i 1) methods)))
                        '()
                        )
                      )))


; get-variables frame)
;(invoke ((java.lang.Class:getMethods frame:class) 0 ) 'getName  )
(define (show-completion parent e ) 
  (let ((popup (javax.swing.JPopupMenu))
        (jlist (javax.swing.JList) )
        (text-area (javax.swing.JTextArea "hello" ))
        )                 
    (popup:addPopupMenuListener
     (object (javax.swing.event.PopupMenuListener)
             ((popupMenuWillBecomeVisible e)
              (text-area:request-focus)
              )
             ((popupMenuWillBecomeInvisible e)
              (newline)
              )
             ((popupMenuCanceled e)
              (newline)
              )))

    (jlist:setListData (apply object[]  get-variables frame ) )  )
    ; (jlist:setPreferredSize (java.awt.Dimension 200 300 ) )
    ; (popup:add (javax.swing.JMenuItem "hello" ))
    ;(popup:add (javax.swing.JMenuItem "hoo" ))
    ;(popup:add (javax.swing.JButton "hoo" ))
    (popup:add text-area)
    (popup:add (javax.swing.JScrollPane jlist ))
    (popup:show parent e:x e:y )))


; Example 
; (show-completion frame:text-pane frame:text-pane:caret:magic-caret-position )


; This creates a window and set a mouselistener to show a popup menu.
(define init-completion (lambda ()
                          (let ((menu-bar (javax.swing.JMenuBar))
                                (this-frame (javax.swing.JFrame 
                                              visible:#t 
                                              size:(java.awt.Dimension 500 500 )
                                              ))
                                )
                            (menu-bar:add (javax.swing.JMenu "hello" ))
                            (this-frame:set-j-menu-bar menu-bar)
                            (this-frame:addMouseListener
                              (object (java.awt.event.MouseAdapter) 
                                      ((mousePressed e)
                                       (display 'hello)
                                       (newline)
                                       (if (e:isPopupTrigger)
                                         (show-completion e:component e )
                                         #f
                                         )
                                       ))))
                          ))
; (init-completion)



; (ats.pulsar.editor.PulsarScratchPad:lookupCorrespondingParenthesis "(hello)" 5 )

;(register-event-handler 'caret 'test)

(define-class jump-to-corresponding-parenthesis-action (javax.swing.text.TextAction)
              ((*init* (name ::java.lang.String)(with-select-0 ::boolean))
               (invoke-special javax.swing.text.TextAction (this) '*init* name)
               (set! with-select with-select-0)
               ((this):putValue javax.swing.Action:ACCELERATOR_KEY (javax.swing.KeyStroke:getKeyStroke 
                                                                    java.awt.event.KeyEvent:VK_G
                                                                    (if with-select
                                                                      (+ java.awt.event.KeyEvent:CTRL_MASK
                                                                         java.awt.event.KeyEvent:SHIFT_MASK )
                                                                      java.awt.event.KeyEvent:CTRL_MASK ))))
              (with-select ::boolean 5)
                                                  
              ;((*init*) #!void)
              ((actionPerformed e)
               (display (this))
               (newline)
               (let* ((text-pane ((this):getTextComponent e ))
                      (pos
                       (ats.pulsar.editor.PulsarScratchPad:lookupCorrespondingParenthesis 
                        text-pane:text 
                        text-pane:caret:dot )
                       ))
                 (if (<= 0 pos)
                   (begin
                    (if with-select 
                      (text-pane:caret:move-dot pos)
                      (set! text-pane:caret:dot pos)
                      )
                    #t)
                   #f)))
              (x 1)
              (init: (begin
                      ((this):putValue javax.swing.Action:NAME        (->java.lang.String "Jump to the Corresponding Parenthesis" ))
                      ((this):putValue ats.pulsar.editor.Action2:NAME (->java.lang.String "Jump to the Corresponding Parenthesis" ))
                      ((this):putValue javax.swing.Action:MNEMONIC_KEY  (->java.lang.Integer (char->integer #\p ) ))
                      )))


(define init-proc (lambda (frame)
               (set! lisp-words (append default-lisp-words
                         '(object append define-class define-simple-class )
                         '(register-event-handler unregister-event-handler )
                           ))

               ((frame:j-menu-bar:get-menu 1):add (javax.swing.JMenuItem (jump-to-corresponding-parenthesis-action "hello1" #t)))
               ((frame:j-menu-bar:get-menu 1):add (javax.swing.JMenuItem (jump-to-corresponding-parenthesis-action "hello2" #f)))

               (frame:text-pane:addMouseListener
                (object (java.awt.event.MouseAdapter) 
                        ((mousePressed e)
                         (display 'hello)
                         (newline)
                         (if (e:isPopupTrigger)
                           (let ((popup (javax.swing.JPopupMenu))
                                 (jlist (javax.swing.JList) )
                                 (text-area (javax.swing.JTextArea "hello" ))
                                 )                 
                             (popup:addPopupMenuListener
                              (object (javax.swing.event.PopupMenuListener)
                                      ((popupMenuWillBecomeVisible e)
                                       (text-area:request-focus)
                                       )
                                      ((popupMenuWillBecomeInvisible e)
                                       (newline)
                                       )
                                      ((popupMenuCanceled e)
                                       (newline)
                                       )))

                             (jlist:setListData (apply object[]  get-variables frame )))
                             ; (jlist:setPreferredSize (java.awt.Dimension 200 300 ) )
                             ; (popup:add (javax.swing.JMenuItem "hello" ))
                             ;(popup:add (javax.swing.JMenuItem "hoo" ))
                             ;(popup:add (javax.swing.JButton "hoo" ))
                             (popup:add text-area)
                             (popup:add (javax.swing.JScrollPane jlist ))
                             (popup:show e:component e:x e:y )
                             )
                           #f
                           )
                         )))))

(register-event-handler 'init 'init-0 init-proc )



; Example of defining a simple class
; (define-simple-class A (javax.swing.text.TextAction)
;                      ((*init* (name ::java.lang.String))
;                       (invoke-special javax.swing.text.TextAction (this) '*init* name))
;                                                   
;                      ;((*init*) #!void)
;                      ((actionPerformed e)
;                       (newline)
;                       )        
;                      )


;(aa:actionPerformed #!null)
;(aa:getValue javax.swing.Action:ACCELERATOR_KEY)
; (display ((this):getValue javax.swing.Action:MNEMONIC_KEY ))
; (newline)

; (frame:j-menu-bar:add
; (javax.swing.JMenu "HELLO"))
; (frame:revalidate)
; primitive-set-static


; type conversion examples
; (->java.lang.String "hello")
; (java.lang.Character->java.lang.Integer (->java.lang.Character  #\p ))
; (char? #\n )
; (char->integer #\n)


#| 
(define-simple-class <my-class> ()
  (allocation: 'class
   init: (perform-actions-when-the-class-is-initizalized))
  (x init: 10)
  (init: (if (some-condition) (set! x (* x 2)))))

|#










; vim:expandtab:
