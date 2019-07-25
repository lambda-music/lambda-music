(load "./pulsar-basic-framework.scm" )

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
