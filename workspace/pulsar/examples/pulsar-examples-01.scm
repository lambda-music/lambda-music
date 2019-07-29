(load "./pulsar-basic-framework.scm" )

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






((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "(map (lambda(x) (/ x 5)) '( 1 1 1 5 ) )" "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Ride-Jazz 'pns-basic-4-swing " (apply circular-list '( 2/4 2/4 2/4 3/4 ) )  " "(+ 0/4 0 )" 4 1))


((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "'( 1/4 3/4 1/4 3/4 )" "(+ 1/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Ride-Rock 'pns-basic-4-swing "(cl '( 2/4 2/4 2/4 4/4 ) )" "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-sn-33 'pns-basic-one "'( 2/4 2/4 2/4 3/4 )" "(+ 0/8 0 )" 1 1))


(if #f 
  (mov! -1/4 (m #f pos: (ap 5 1 ) ) )
  )

