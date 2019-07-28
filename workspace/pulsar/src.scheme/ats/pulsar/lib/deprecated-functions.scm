
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Defining Utilities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Defining a fundamental function No.1 Delegator
(define-syntax new-delegator
  (syntax-rules ()
                ((new-delegator proc )
                 (lambda args
                   (apply (eval 'proc) args)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Defining a fundamental function No.2 "invoker"

(define new-invoker (lambda args
                      (lambda args-0
                        (apply (car args) (cdr args) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax new-seq-putter
  (syntax-rules ()
                ((new-seq-putter name proc args ... )
                 (begin
                    ; (display-args args ... )(newline)
                    (new-invoker put-seq! name (new-delegator proc) args ... )))))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define centering-notes-example 
  (lambda()
    (centering-notes! 'Z 3/4 
                     (append
                       (list
                         (hhp1 'A   #f  1/4 0.7 )
                         (hhp1 'A   #f  3/4 0.7 )

                         (rid1 '(A  ) (luck 1.00 ) (+ 0/4 0   ) (+ 0.5  (rnd -0.0  0.0 )))
                         (rid1 '(A  ) (luck 0.50 ) (+ 0/4 1/8 ) (+ 0.75 (rnd -0.0  0.0 )))

                         (rid1 '(A  ) (luck 1.00 ) (+ 1/4 0   ) (+ 0.5  (rnd -0.0  0.0 )))
                         (rid1 '(A  ) (luck 1.00 ) (+ 1/4 1/8 ) (+ 0.75 (rnd -0.0  0.0 )))

                         (rid1 '(A  ) (luck 1.00 ) (+ 2/4 0   ) (+ 0.5  (rnd -0.0  0.0 )))
                         (rid1 '(A  ) (luck 0.50 ) (+ 2/4 1/8 ) (+ 0.75 (rnd -0.0  0.0 )))

                         (rid1 '(A  ) (luck 1.00 ) (+ 3/4 0   ) (+ 0.5  (rnd -0.0  0.0 )))
                         (rid1 '(A Z) (luck 1.00 ) (+ 3/4 1/8 ) (+ 0.75 (rnd -0.0  0.0 )))

                         (len 1.0 ))))))

 (display-notes #f (centering-notes-example))

(display-notes #f
                 (cpy
                   (cut mov! 10 <>)
                   (list
                     (hhp1 'A   #f  1/4 0.7 )
                     (hhp1 'A   #f  3/4 0.7 )

                     (rid1 '(A  ) (luck 1.00 ) (+ 0/4 0   ) (+ 0.5  (rnd -0.0  0.0 )))
                     (rid1 '(A  ) (luck 0.50 ) (+ 0/4 1/8 ) (+ 0.75 (rnd -0.0  0.0 )))

                     (rid1 '(A  ) (luck 1.00 ) (+ 1/4 0   ) (+ 0.5  (rnd -0.0  0.0 )))
                     (rid1 '(A  ) (luck 1.00 ) (+ 1/4 1/8 ) (+ 0.75 (rnd -0.0  0.0 )))

                     (rid1 '(A  ) (luck 1.00 ) (+ 2/4 0   ) (+ 0.5  (rnd -0.0  0.0 )))
                     (rid1 '(A  ) (luck 0.50 ) (+ 2/4 1/8 ) (+ 0.75 (rnd -0.0  0.0 )))

                     (rid1 '(A  ) (luck 1.00 ) (+ 3/4 0   ) (+ 0.5  (rnd -0.0  0.0 )))
                     (rid1 '(A Z) (luck 1.00 ) (+ 3/4 1/8 ) (+ 0.75 (rnd -0.0  0.0 )))

                     (len 1.0 ))))
(newline)

|#
