(load "./pulsar-basic-framework.scm" )



(send2! 
  (n chan: 0 port: 2 
     
     (melody '( o 4 do 1/5 re mi fi sol re , mi ' fi sol la mi , fi ' sol la ti end ))))

(mov! -1/4 (m #f pos: (ap 5 1 ) ) )



































((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-rid1 'pns-basic-4-swing "(+ -1/8 0 )" 5 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-snar2 'pns-basic-one "(+ -1/8 0 )" 1 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-csh1 'pns-basic-one "(+ 0/4 0 )" 1 4))

((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-rid1 'pns-basic-4-swing "(+ -1/8 0 )" 5 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-snar2 'pns-basic-one "(+ -1/8 0 )" 1 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-csh1 'pns-basic-one "(+ 0/4 0 )" 1 4)
                                  (xnew SimpleTrack (new-track-id) 'inst-drys 'pns-basic-one "(+ 0/4 0 )" 8 3))

((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-rid1 'pns-basic-4-swing "(+ -1/8 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-snar2 'pns-basic-one "(+ -1/8 0 )" 1 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-csh1 'pns-basic-one "(+ 0/4 0 )" 1 4)
                                  (xnew SimpleTrack (new-track-id) 'inst-drys 'pns-basic-one "(+ 0/4 0 )" 4 5))

((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 7 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-rid1 'pns-basic-4-swing "(+ -1/8 0 )" 5 2)
                                  (xnew SimpleTrack (new-track-id) 'inst-snar2 'pns-basic-one "(+ -1/8 0 )" 5 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-csh1 'pns-basic-one "(+ 0/4 0 )" 1 4)
                                  (xnew SimpleTrack (new-track-id) 'inst-drys 'pns-basic-one "(+ 0/4 0 )" 16 1))
(if (not (open?))
  (begin
   ((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-snar2 'pns-basic-one "(+ -1/4 0 )" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-csh1 'pns-basic-one "(+ 0/4 0 )" 1 4)
                                  (xnew SimpleTrack (new-track-id) 'inst-snar 'pns-basic-one "(+ -3/8 0 )" 1 4)
                                  (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ -1/8 0 )" 1 1))
   ((trackset-manager 'new-trackset) 'add-track 
                                     (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                     (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 4 1))
   
   ((trackset-manager 'new-trackset) 'add-track 
                                     (xnew SimpleTrack (new-track-id) 'inst-rid1 'pns-basic-one "(+ -1/8 0 )" 1 2)
                                     (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                     (xnew SimpleTrack (new-track-id) 'inst-drys 'pns-basic-one "(+ -1/4 0 )" 2 1))


   ((trackset-manager 'new-trackset) 'add-track 
                                     (xnew SimpleTrack (new-track-id) 'inst-drys 'pns-basic-one "(+ -1/4 0 )" 2 1)
                                     (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                     (xnew SimpleTrack (new-track-id) 'inst-rid1 'pns-basic-one "(+ -1/8 0 )" 1 2))

   ((trackset-manager 'new-trackset) 'add-track 
                                     (xnew SimpleTrack (new-track-id) 'inst-drys 'pns-basic-one "(+ -1/4 0 )" 2 1)
                                     (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                     (xnew SimpleTrack (new-track-id) 'inst-rid1 'pns-basic-one "(+ -1/8 0 )" 1 2))

   ((trackset-manager 'new-trackset) 'add-track 
                                     (xnew SimpleTrack (new-track-id) 'inst-csh1 'pns-basic-one "(+ -1/8 0 )" 1 2)
                                     (xnew SimpleTrack (new-track-id) 'inst-kikl 'pns-basic-one "(+ -1/8 0 )" 2 1)
                                     (xnew SimpleTrack (new-track-id) 'inst-snar2 'pns-basic-one "(+ -1/4 0 )" 2 1))
   )
  )
