(load "./pulsar-basic-framework.scm" )

; (gui-get-user-object (gui-get (part1 'gui) 'self) )
; (remove-all-parts)
; (add-all-parts part1 part2 part3)
; (all-parts-to-lisp )

; ((car main-frame):pack)
; ((car main-pane):setPreferredSize (java.awt.Dimension 1000 500) )
; ((car main-frame):pack)
; ((car main-frame):pack)

#|
(add-all-parts 
 (xnew Part (quote part6) (quote inst-kikl) (quote pns-basic-one) " (+ 0/4 0 ) " (quote 4) (quote 1))
 (xnew Part (quote part5) (quote inst-kikl) (quote pns-basic-one) " (+ 0/4 0 ) " (quote 4) (quote 1))
 (xnew Part (quote part4) (quote inst-kikl) (quote pns-basic-one) " (+ 0/4 0 ) " (quote 4) (quote 1))
 (xnew Part (quote part3) (quote inst-kikl) (quote pns-basic-one) " (+ 0/4 0 ) " (quote 4) (quote 1))
 (xnew Part (quote part2) (quote inst-kikl) (quote pns-basic-one) " (+ 0/4 0 ) " (quote 4) (quote 1))
 (xnew Part (quote part1) (quote inst-kikl) (quote pns-basic-one) " (+ 0/4 0 ) " (quote 4) (quote 1))) 
|#

; (remove-all-parts)

;(list->string (list "1" "2" "3" ) )
; (apply string-append (map symbol->string '( hello world ) ) )

; (symbol->string-rec '( 1 2 3 ) )




(if #f (begin
        (define f (gui-new 'frame))
        (f:setVisible #t)
        (f:setSize  (java.awt.Dimension 500 500 ))
        (gui-build!
         (f:getContentPane)
         (lambda (self) (self:setLayout (javax.swing.BoxLayout self javax.swing.BoxLayout:Y_AXIS ))) 
         ; (gui-new 'button "HELLO" (lambda args (newline) ))
         ;(gui-new 'button "HELLO" (lambda args (newline) ))
         (gui-build!
          (gui-new 'group "HELLO" 'flow )
          ;(lambda (self) (self:setLayout (ats.pulsar.lib.swing.FlawLayout ) )  )
          (lambda (self)
            (self:setMaximumSize (java.awt.Dimension 3000 300 )))           
          (gui-new 'button "hello" (lambda args (newline) ))
          (gui-new 'button "hello" (lambda args (newline) ))

          )   
         (gui-new 'button "HELLO" (lambda args (newline) ))
         )
        ))

((object (ats.pulsar.lib.swing.JNamedPanel) 
        ((test) (begin (display "hello" )(newline)))
        ):test)

(object (ats.pulsar.lib.swing.JNamedPanel) 
        ((test) (begin (display "hello" )(newline)))
        ((getPreferredSize) (invoke-special javax.swing.JComponent (this) 'getMaximumSize)))




(object (ats.pulsar.lib.swing.JNamedPanel) 
                ((test) (begin (display "hello" )(newline)))
                #|
                ((getMinimumSize)
                 (let ((s (invoke-special javax.swing.JComponent (this) 'getMaximumSize)))
                   (set! s:height (- s:height 50 ) )
                   (set! s:width  (- s:width  50 ) )
                   (display s)
                   (newline)
                   s))
                |#
                )

(define-simple-class <my-class> ()
  (allocation: 'class
   init: (perform-actions-when-the-class-is-initizalized))
  (x init: 10)
  (init: (if (some-condition) (set! x (* x 2)))))

