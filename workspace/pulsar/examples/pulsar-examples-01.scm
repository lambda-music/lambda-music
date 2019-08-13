(if (not (open?))
  (begin
   (load "./ats's-drumkit.scm" )
   (load "./pulsar-pattern-generator.scm" )
   (create-gui)
   ))

(open "hello")
(openi "foo")
(openi '( bar ))
(close)

(openo "a6" "a5" )
(putt (n chan: 11 port: 0 type: 'note 
         velo: << (map / (iota 8 1 ) (cl 8))
         pos: >> (map / (iota 8 0) (cl 8)) (len 1) ))
(putt (n chan: 11 port: 0 type: 'note 
         velo: 1
         note: 80
         pos: >> 0 ))
(remt (lst))

(set-tempo 160)

(openo '( "aaa1" "aaa2") '( hello world)  )

(lso)
(lst)
(closeo (lso))
(list-ref '(1 2 3) 2 )
(lsi)

(remt (lst))
(apply closei (lsi))
(openo 'foo 'bah)
(closeo 'foo 'bah)

; (quit!)

(putt (newt  (n chan: 1 port: 2 (melody '(ti do re mi fi sol la ti end )) )))





((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Ride-Bell 'pns-basic-one "(cl 1/4 1/4 3/4 )" "(+ -0/4 -0/32)" 9 2)
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 3/4 3/4 3/4 2/4 )" "(+ -0/4 -0/32)" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-sn-33 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ -1/4 -0/32)" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-SB-Kik22psoA1 'pns-two-four "(cl 2/4 3/4 1/4 4/4 )" "(+ -1/36 -0/32)" 9 1))


(if #f (begin

((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ -0/4 -0/32)" 1 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Hand-Clap 'pns-basic-one "(cl 4/4 )" "(+ -1/4 -1/12 2/36   )" 1 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Crash-Jazz 'pns-basic-one "(cl 1/4 2/4 1/4 3/4 )" "(+ -0/4 -0/32)" 8 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 4/4 )" "(+ -3/16 -0/32)" 1 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Snare2 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ -1/4 -0/32)" 2 1))

((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Sizzle-Cymbal 'pns-basic-one "'( 1/4 3/4 1/4 3/4 2/4 3/4 4/4 )" "(- 1/7 1/4 )" 7 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-SB-Ride2Brush 'pns-basic-one "(cl '( 2/4 2/4 2/4 4/4 ) )" "(+  1/8 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Hand-Clap 'pns-two-four "'( 2/4 2/4 2/4 3/4 )" "(+ 0/8 0 )" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-counting "'( 2/4 2/4 2/4 3/4 )" "(- 1/7 1/4 )" 7 1))

((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 1/4 3/4 1/4 3/4 )" "(+ -1/4 0 )" 5 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Mid-Tom 'pns-basic-one "(cl '( 2/4 2/4 2/4 4/4 ) )" "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-sn-33 'pns-basic-one "'( 2/4 3/4 2/4 3/4 2/4 3/4 2/4 4/4 )" "(+ 0/8 0 )" 8 1))

((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 1/8 3/8 1/8 4/4 )" "(+ 1/4 0 )" 10 2)
                                  (xnew SimpleTrack (new-track-id) 'inst-Paiste-Ride 'pns-basic-4-swing "(cl '( 2/4 2/4 2/4     2/4 2/4 4/4 ) )" "(+ 0/4 0 )" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-sn-33 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ -2/8 0 )" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-SB-TomB12in 'pns-basic-4-swing "'( 2/4 2/4 2/4 3/4 )" "(+ -0/4 -0/32)" 5 2))
; Seven Two
((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 0/4 3/4 1/4 4/4 )" "(+ 1/4 0 )" 7 2)
                                  (xnew SimpleTrack (new-track-id) 'inst-Ride-Rock 'pns-basic-4-swing "(cl '( 2/4 2/4 2/4 4/4 ) )" "(+ 0/4 0 )" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-sn-33 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ -2/8 0 )" 1 1))

((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 1/4 3/4 1/4 3/4 )" "(+ 0/4 0 )" 4 2)
                                  (xnew SimpleTrack (new-track-id) 'inst-Ride-Rock 'pns-basic-4-swing "(cl '( 2/4 2/4 2/4 4/4 ) )" "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-sn-55 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ -1/4 0 )" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ -3/16 0 )" 1 1))


((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "(map (lambda(x) (/ x 5)) '( 1 1 1 5 ) )" "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Ride-Jazz 'pns-basic-4-swing " (apply circular-list '( 2/4 2/4 2/4 3/4 ) )  " "(+ 0/4 0 )" 4 1))


((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 1/4 3/4 1/4 3/4 )" "(+ 1/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Ride-Rock 'pns-basic-4-swing "(cl '( 2/4 2/4 2/4 4/4 ) )" "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-sn-33 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ 0/8 0 )" 1 1))


))
(if #f (begin
        (send2! 
         (n chan: 0 port: 2 
            (melody '( o 4 do 1/5 re mi fi sol re , mi ' fi sol la mi , fi ' sol la ti end ))))


        ((trackset-manager 'new-trackset) 'add-track 
                                          (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                          (xnew SimpleTrack (new-track-id) 'inst-Hand-Clap 'pns-basic-one "(+ -1/4 0 )" 2 1)
                                          (xnew SimpleTrack (new-track-id) 'inst-SB-Crash2BA1 'pns-basic-one "(+ -1/8 0 )" 1 4))

        ))

(take (apply circular-list '( 2/4 2/4 2/4 3/4 ) ) 10 )




(if #f 
  (mov! -1/4 (m #f pos: (ap 5 1 ) ) )
  )

