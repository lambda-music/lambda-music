; 
; Kawapad written by Atsushi Oka 
; Copyright 2018 Atsushi Oka
; 
; This file is part of Metro Musical Sequencing Framework. 
; 
; Kawapad is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; Kawapad is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with Kawapad.  If not, see <https://www.gnu.org/licenses/>.
; 

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

; =================================================================================
; This file is designed to be executed only once at booting up of the application.
; =================================================================================


; Add the .kawapad configuration directory to import path.  The configuration
; directory should be on the user's home directory.
; (java.lang.System:set-property "kawa.import.path"
;                                (string-append (java.lang.System:get-property "kawa.import.path" "")
;                                               ":"
;                                               (java.lang.System:get-property "user.home" "")
;                                               "/.kawapad"))
; 
; (Wed, 07 Aug 2019 17:45:48 +0900)
(java.lang.System:set-property "kawa.import.path"
                               (string-append 
                                 (let ((p (java.lang.System:get-property "kawa.import.path" ) ))
                                   (if (eq? p #!null) "" (string-append p ":" )))
                                 (let ((p (java.lang.System:get-property "user.home")))
                                   (if (eq? p #!null) "" (string-append p "/" )))
                                 ".kawapad"))

(define **display-warn (lambda args 
                       (values)))
(define **newline-warn (lambda args 
                       (values)))


(define get-variables (lambda (this-object)
                    ;(**display-warn this-object)
                    ;(**newline-warn)
                    ;(**display-warn this-object:class)
                    ;(**newline-warn)
                    (let loop ((i 0)
                               ( methods (java.lang.Class:getMethods this-object:class) ))
                      (if (and (< i methods:length ) (not (eq? (methods i) #!null )))     
                        (let ((name (invoke (methods i) 'getName  )))
                          (**display-warn name)
                          (**newline-warn)
                          (cons name (loop (+ i 1) methods)))
                        '()
                        )
                      )))


; get-variables frame)
;(invoke ((java.lang.Class:getMethods frame:class) 0 ) 'getName  )
; (define (show-completion parent e ) 
;   (let ((popup (javax.swing.JPopupMenu))
;         (jlist (javax.swing.JList) )
;         (text-area (javax.swing.JTextArea "hello" ))
;         )                 
;     (popup:addPopupMenuListener
;      (object (javax.swing.event.PopupMenuListener)
;              ((popupMenuWillBecomeVisible e)
;               (text-area:request-focus)
;               )
;              ((popupMenuWillBecomeInvisible e)
;               (**newline-warn)
;               )
;              ((popupMenuCanceled e)
;               (**newline-warn)
;               )))
; 
;     (jlist:setListData (apply object[]  (get-variables frame ) )  )
;     ; (jlist:setPreferredSize (java.awt.Dimension 200 300 ) )
;     ; (popup:add (javax.swing.JMenuItem "hello" ))
;     ;(popup:add (javax.swing.JMenuItem "hoo" ))
;     ;(popup:add (javax.swing.JButton "hoo" ))
;     (popup:add text-area)
;     (popup:add (javax.swing.JScrollPane jlist ))
;     (popup:show parent e:x e:y )))
; 

; Example 
; (show-completion frame:text-pane frame:text-pane:caret:magic-caret-position )




#|
(define-class class-show-completion (javax.swing.AbstractAction) 
              ((actionPerformed e)
               (**display-warn 'hello)
               (**newline-warn)
               )
              (init: (begin
                       (**display-warn 'hello-init)
                       ))
              )

|#

; don't execute it now.
; (class-show-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (pulsar.editor.PulsarScratchPad:lookupCorrespondingParenthesis "(hello)" 5 )

;(register-event-handler 'caret 'test)





; ========================================================
; init-proc is called whenever a scheme object is created.
; ========================================================
(define init-proc (lambda (frame)
                    (set! lisp-words (append default-lisp-words
                                             '(object append define-class define-simple-class )
                                             '(register-event-handler unregister-event-handler )
                                             ))))

(register-event-handler 'init 'init-0 init-proc )


; ========================================================
; create-proc is called whenever a FRAME object is created.
; ========================================================
(define create-proc (lambda (frame)
                      (**display-warn "==================")
                      (**display-warn 'create-proc)
                      (**display-warn "==================")
                      (**newline-warn)
                      ; ========================================================
                      ; Note : all procedures that register event handlers should be executed in (create-proc).
                      ; (Tue, 06 Aug 2019 17:36:50 +0900)
                      ; ========================================================

                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                      ; This creates a window and set a mouselistener to show a popup menu.
                      ;(define init-completion (lambda ()
                      ;                          (let ((menu-bar (javax.swing.JMenuBar))
                      ;                                (this-frame (javax.swing.JFrame 
                      ;                                              visible:#t 
                      ;                                              size:(java.awt.Dimension 500 500 )
                      ;                                              ))
                      ;                                )
                      ;                            (menu-bar:add (javax.swing.JMenu "hello" ))
                      ;                            (this-frame:set-j-menu-bar menu-bar)
                      ;                            (this-frame:addMouseListener
                      ;                              (object (java.awt.event.MouseAdapter) 
                      ;                                      ((mousePressed e)
                      ;                                       ; (**display-warn 'hello)
                      ;                                       ; (**newline-warn)
                      ;                                       (if (e:isPopupTrigger)
                      ;                                         (show-completion e:component e )
                      ;                                         #f
                      ;                                         )
                      ;                                       ))))
                      ;                          ))
                      ; This class is not completed. Don't execute it.
                      ; (Tue, 06 Aug 2019 17:35:09 +0900)
                      ; (init-completion)

                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                      (define-class jump-to-corresponding-parenthesis-action (javax.swing.text.TextAction)
                                    ((*init* (name ::java.lang.String)(with-select-0 ::boolean))
                                     (invoke-special javax.swing.text.TextAction (this) '*init* name)
                                     (set! with-select with-select-0)
                                     ((this):putValue javax.swing.Action:ACCELERATOR_KEY (javax.swing.KeyStroke:getKeyStroke 
                                                                                           java.awt.event.KeyEvent:VK_D ; modified (Tue, 13 Aug 2019 16:36:55 +0900)
                                                                                           (if (not with-select)        ; modified (Tue, 13 Aug 2019 16:36:55 +0900)
                                                                                             (+ java.awt.event.KeyEvent:CTRL_MASK
                                                                                                java.awt.event.KeyEvent:SHIFT_MASK )
                                                                                             java.awt.event.KeyEvent:CTRL_MASK ))))
                                    (with-select ::boolean 5)

                                    ;((*init*) #!void)
                                    ((actionPerformed e)
                                     ; (**display-warn (this))
                                     ; (**newline-warn)
                                     (let* ((text-pane ((this):getTextComponent e ))
                                            (pos
                                              (kawapad.KawaPad:lookupCorrespondingParenthesis 
                                                text-pane:text 
                                                text-pane:caret:dot )
                                              ))
                                       (**display-warn "Parenthesis Jump:" )
                                       (**display-warn pos)
                                       (**newline-warn)
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
                                             ((this):putValue kawapad.lib.Action2:NAME (->java.lang.String "Jump to the Corresponding Parenthesis" ))
                                             ((this):putValue javax.swing.Action:MNEMONIC_KEY  (->java.lang.Integer (char->integer #\p ) ))
                                             )))

                      ((frame:j-menu-bar:get-menu 1):add (javax.swing.JMenuItem (jump-to-corresponding-parenthesis-action "hello1" #t)))
                      ((frame:j-menu-bar:get-menu 1):add (javax.swing.JMenuItem (jump-to-corresponding-parenthesis-action "hello2" #f)))

                      (**display-warn 'frame:text-pane:addMouseListener )
                      (**newline-warn)
                      (frame:text-pane:addMouseListener
                        (object (java.awt.event.MouseAdapter) 
                                ((mousePressed e)
                                 (**display-warn "hello-hello")
                                 (**newline-warn)
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
                                                (**newline-warn)
                                                )
                                               ((popupMenuCanceled e)
                                                (**newline-warn)
                                                )))

                                     (jlist:setListData (apply object[]  get-variables frame ))
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

(register-event-handler 'create 'create-0 create-proc )


; Example of defining a simple class
; (define-simple-class A (javax.swing.text.TextAction)
;                      ((*init* (name ::java.lang.String))
;                       (invoke-special javax.swing.text.TextAction (this) '*init* name))
;                                                   
;                      ;((*init*) #!void)
;                      ((actionPerformed e)
;                       (**newline-warn)
;                       )        
;                      )


;(aa:actionPerformed #!null)
;(aa:getValue javax.swing.Action:ACCELERATOR_KEY)
; (**display-warn ((this):getValue javax.swing.Action:MNEMONIC_KEY ))
; (**newline-warn)

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
