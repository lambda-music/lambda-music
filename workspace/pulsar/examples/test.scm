(load "./ats's-drumkit.scm" )
(play)
(close)
(quit!)
(list? (ls))
(define f (car (lso)))
(putt (newt 'hello (n port: 0 (melody '(do re mi sol end )))))
(remt (gett 'hello) )

(remt (gett 'hello1) 's)
(putt (newt 'hello1 (n             chan: 1 port: 0 (melody '( do re mi sol end ) ) ) ) 'serial (gett 'main))  
(putt (newt 'hello1 (n             chan: 1 port: 2 (melody '( do re mi sol end ) ) ) ) 'serial (gett 'main))  
(putt (newt 'count1 
            (n 
              (n type: 'note chan: 1 port: 1 
                 velo: << (cl 0.8) 
                 len:  << (cl 1/4 ) 
                 pos:  << (cl 0/4 1/4 2/4 3/4 4/4 )
                 note: >> 60 61 62 63 64  )
              (n type: 'len val: 5/4 ))
            )
      's (gett 'main))

(remt (gett  'hello1  ))
(remt (gett  'count1  ))
(n  pos: >> 1 2 3  )
(ap 5 2)
(ls)
(remt (gett 'hello1  ))
(gett 'hello1 'hello2 )
(remt (gett 'hello2 ))



trackset-manager

(send!
  (n chan: 0 port: 0 
     (p
       (p type: 'note chan: 0 port: 0 len: 0.5 pos: 0 velo: 1.0 note: >> (iota 400 ) )
       (p type: 'len val: 1 )
       (n type: 'end )
       )

     (s
       (repeat 2 (melody '( o 4 sol 1/32 re mi sol end )))
       (melody '( o 4 do 1/32 re mi sol end ))
       (melody '( o 4 do 1/32 re mi sol end ))
       )
     )
  )

(ls)
(del! 'seq-1 #f)

(kill! 'seq-14 'seq-15)
(kill! 'seq-7 'seq-8 'seq-9 'seq-10 'seq-11)

(add! 'hello 
  (n (n chan: 0 port: 2 (melody '( do re mi fa sol end ) ) )))

(ls)
(del! 'hello)

(list-tabulate 4 make-list)


(append-notes
  (melody '( o 4 do 1/32 re mi sol end ))
  (melody '( o 4 do 1/32 re mi sol end )))


(n chan: 0 port: 0 (n note: 1 )(n note: 1 )  )
(n chan: 0 port: 0 (melody '( do re mi end ) )  )

(p chan: 0 port: 0 note: << (cl  3 2 1 ) velo: >> '( 1 2 3 )  )

(play!)
(add-all! (list 'm1
                (n chan: 0 port: 0 
                   (melody '( do re mi sol end )))
                'parallel 'main )
          (list 'm2
                (n chan: 0 port: 0 
                   (melody '( mi fa sol ti end )))
                'parallel 'main ))

(add-all! (list 'm1
                (n chan: 0 port: 0 
                   (melody '( do re mi sol end )))
                'immediate  )
          (list 'm2
                (n chan: 0 port: 0 
                   (melody '( mi fa sol ti end )))
                'immediate  ))

(add! 'm1 (list (n type: 'exec pos: 0.4 proc: (lambda()
                                                (display 'hello)
                                                (newline)
                                                ))
                (n type: 'end )
                ))

(add! 'm1 (n type: note pos: >> (ap 4 1) ) )


(add-all!  
  (list 'm1    (n chan: 0 port: 0 type: 'note note: 60 velo: << (cl 0.9 0.4 0.4 0.4 )  pos: >> (ap 4 1)             ) )
  (list 'm2 (p (n chan: 0 port: 0 type: 'note note: 68 velo: << (cl 0.8 )              pos: >> (ap 7 3)  ) (len 3 ) ) )
  )


(kill! 'm1 'm2 )
(del! 'm1 'm2 #f )
(list (len 1) )
(add! 'main (list (len 1) ) )
(ls)


(let ((bar-count 17 )
      (note-name 73))
  (add! 'he2 
        (n type: 'note chan: 0 port: 0 
           len: 0.1
           velo: << 
           (reverse
             (map
               (lambda (x)
                 (+ (/ x 2 )
                    0.5 )
                 )
               (map cos (map * (iota 10000 ) (cl (* (acos -1) 1/10 ) )) )))

           pos: << (ap 11 3 )
           ; (map * (iota bar-count) (cl (/ 1 bar-count) ))
           note: >> (make-list bar-count note-name ) )

        'parallel 'main
        ))
(ls)
(kill! 'he2)

(let ((bar-count 11 ))
  (add! 'he 
        (n type: 'note chan: 0 port: 0 
           len: 0.1
           velo: << 
           (reverse
             (map
               (lambda (x)
                 (+ (/ x 2 )
                    0.5 )
                 )
               (map cos (map * (iota 1000 ) (cl (* (acos -1) 1/10 ) )) )))

           pos: << (map * (iota bar-count) (cl (/ 1 bar-count) ))
           note: >> (make-list bar-count 63 ) )

        'parallel 'main
        ))

(play!)

(ls)

(kill! 'seq-1)

(acos -1)
(take (cl 1 2 3) 3)
(take (cl 1 2 3) 4 )
(take (cl 1 2 3 ) 3 )

(put! 'cnt 
      (n
        (n type: 'note port: 1 chan: 0 velo: 0.7 len: 0.5 note: << (iota 10 60)  pos: >> (ap 10 3 ) )
        (n type: 'len val: 3 )) 'parallel 'main
      )
(kill! 'cnt)

(put! 'cnt22
          (n
            (n type: 'note port: 1 chan: 0 velo: 0.7 len: 0.5 note: << (iota 10 60)  pos: >> (ap 10 5 ) )
            (n type: 'len val: 2 )) 'parallel 'main
          )

(kill! 'cnt22 )


(put! 'he1
  (n chan: 0 port: 2  velo: 1
                   (repeat 2 (melody '( o 3 do 1   re mi sol r   end )))
                   (repeat 6 (melody '( o 4 mi 1/3 fi sol ti r   end )))) 'parallel 'main) 
(ls)
(open! "")


(kill! 'seq-2)
(del! 'track-1)

(snd!
  (n chan: 0 port: 0 
     (s
       (repeat 2 (melody '( o 4 sol 1/32 re mi sol end )))
       (melody '( o 4 do 1/32 re mi sol end ))
       (melody '( o 4 do 1/32 re mi sol end ))
       (melody '( o 4 do 1/32 re mi sol end ))
       (melody '( o 4 do 1/32 re mi sol end ))
       (repeat 2 (melody '( o 4 sol 1/32 re mi sol end )))
       )
     ))

(send!
  (n chan: 0 port: 0 
     (p
       (p type: 'note chan: 0 port: 0 len: 0.5 pos: 0 velo: 1.0 note: >> (iota 200 ) )
       (p type: 'len val: 1 ))

     (s
       (repeat 2 (melody '( o 4 sol 1/32 re mi sol end )))
       (melody '( o 4 do 1/32 re mi sol end ))
       (melody '( o 4 do 1/32 re mi sol end ))
       )
     ))

(send2!  (n (n chan: 0 port: 0 (repeat 16 (melody '( o 4 do 1/32 r  r sol end ))))))
(send2!  (n (n chan: 5 port: 2 (melody '( o 4 mi sol ti end )) (melody '( o 4 do mi sol end )))))


(send2! 
  (n chan: 0 port: 2 
     (melody '( o 4 do 1/5 re mi fi sol re , mi ' fi sol la mi , fi ' sol la ti end ))))



















; vim: expandtab :
