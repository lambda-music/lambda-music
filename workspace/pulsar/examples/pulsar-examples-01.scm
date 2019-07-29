(load "./pulsar-basic-framework.scm" )

(send2! 
 (n chan: 0 port: 2 
    (melody '( o 4 do 1/5 re mi fi sol re , mi ' fi sol la mi , fi ' sol la ti end ))))


((trackset-manager 'new-trackset) 'add-track 
                                  (xnew SimpleTrack (new-track-id) 'inst-Kick-Long 'pns-basic-one "(+ 0/4 0 )" 4 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-Hand-Clap 'pns-basic-one "(+ -1/4 0 )" 2 1)
                                  (xnew SimpleTrack (new-track-id) 'inst-SB-Crash2BA1 'pns-basic-one "(+ -1/8 0 )" 1 4))

#|
ats.pulsar.lib.swing.JNamedPanel[,5,190,990x305,invalid,layout=javax.swing.BoxLayout,alignmentX=0.0,alignmentY=0.0,border=javax.swing.border.TitledBorder@2a17ff46,flags=9,maximumSize=,minimumSize=,preferredSize=] ats.pulsar.lib.swing.JNamedPanel[,5,190,990x305,invalid,layout=javax.swing.BoxLayout,alignmentX=0.0,alignmentY=0.0,border=javax.swing.border.TitledBorder@2a17ff46,flags=9,maximumSize=,minimumSize=,preferredSize=] ats.pulsar.lib.swing.JNamedPanel[,5,190,990x305,invalid,layout=javax.swing.BoxLayout,alignmentX=0.0,alignmentY=0.0,border=javax.swing.border.TitledBorder@2a17ff46,flags=9,maximumSize=,minimumSize=,preferredSize=] ()
|#







 










(if #f 
  (mov! -1/4 (m #f pos: (ap 5 1 ) ) )
  )

