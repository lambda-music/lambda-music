(frame:eventHandlers:register scheme 'init 'hello (lambda(x)
                                            (display x)
                                            (newline)
                                            ))


(frame:eventHandlers:register 'typed 'test
                              (lambda (x) 
                                (display 'hello)
                                (newline)
                                (display 'scheme)
                                (display scheme)
                                (newline)
                                (display 'frame)
                                (display frame:frameName)
                                (newline)
                                ))






(frame:eventHandlers:register 'typed 'test
                              (lambda (x) 
                                (display "===============")
                                (newline)
                                (display 'hello)
                                (newline)
                                (display 'scheme)
                                (display (scheme:hashCode))
                                (newline)
                                (display 'frame-id )
                                (display '\ )
                                (display (eval 'frame):frameName)
                                (display '\ )
                                (display (environment:hashCode))
                                (newline)
                                (display(java.lang.Thread:currentThread) )
                                (newline)
                                ))

                               (display(java.lang.Thread:currentThread))
                               (newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(frame:j-menu-bar:add
(javax.swing.JMenu "HELLO"))
(frame:revalidate)
primitive-set-static



(define get-names (lambda (this-object)
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

;(get-names frame)
;(invoke ((java.lang.Class:getMethods frame:class) 0 ) 'getName  )

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
              (let ((popup (javax.swing.JPopupMenu))
                    (jlist (javax.swing.JList) )
                    )
                (jlist:setListData (apply object[] (get-names this-frame ) )  )
                ; (jlist:setPreferredSize (java.awt.Dimension 200 300 ) )
                (popup:add (javax.swing.JMenuItem "hello" ))
                ;(popup:add (javax.swing.JMenuItem "hoo" ))
                ;(popup:add (javax.swing.JButton "hoo" ))
                (popup:add (javax.swing.JScrollPane jlist ))
                (popup:show e:component e:x e:y )
                )
              #f
              )
            ))))



