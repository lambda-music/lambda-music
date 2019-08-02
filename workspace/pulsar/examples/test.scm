
(list-tabulate 4 make-list)


(append-notes
  (melody '( o 4 do 1/32 re mi sol end ))
  (melody '( o 4 do 1/32 re mi sol end )))


(n chan: 0 port: 0 (n note: 1 )(n note: 1 )  )
(n chan: 0 port: 0 (melody '( do re mi end ) )  )

(p chan: 0 port: 0 note: << (cl  3 2 1 ) velo: >> '( 1 2 3 )  )

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


(ls)
(rm! 'he)

(acos -1)
(take (cl 1 2 3) 3)
(take (cl 1 2 3) 4 )
(take (cl 1 2 3 ) 3 )

(put! 'cnt 
      (n
        (n type: 'note port: 1 chan: 0 velo: 0.7 len: 0.5 note: << (iota 10 60)  pos: >> (ap 10 3 ) )
        (n type: 'len val: 3 )) 'parallel 'main
      )
(rm! 'cnt)

(put! 'cnt22
          (n
            (n type: 'note port: 1 chan: 0 velo: 0.7 len: 0.5 note: << (iota 10 60)  pos: >> (ap 10 5 ) )
            (n type: 'len val: 2 )) 'parallel 'main
          )

(rm! 'cnt22 )


(put! 'he1
  (n chan: 0 port: 2  velo: 1
                   (repeat 2 (melody '( o 3 do 1   re mi sol r   end )))
                   (repeat 6 (melody '( o 4 mi 1/3 fi sol ti r   end )))) 'parallel 'main) 
(ls!)

(rm! 'he1)

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

