( aa )
(((((java.lang.Class:get-protection-domain
     pulsar.Pulsar):get-code-source):get-location):to-URI):get-path)

( a a )

( mov! 0 (sca! 1/1
      (velo: << notation (iota 16) pos: >> (cl 0.6 ) )))
#|

10 ' ( ( ('pos . 20) ( .   (('pos . 11) ('velo . 0.6))
 ('velo . 0.6) (('pos . 12) ('velo . 0.6)) (('pos . 13) ('velo . 0.6))
 (('pos . 14) ('velo . 0.6)) (('pos . 15) ('velo . 0.6))
 (('pos . 16) ('velo . 0.6)) (('pos . 18) ('velo . 0.6))
 (('pos . 17) ('velo . 0.6)) (('pos . 19) ('velo . 0.6))
 (('velo . 0.6)) (('pos . 21) ('velo . 0.6))
 (('pos . 22) ('pos . 23)) (('velo . 0.6) ('velo . 0.6))
 (('pos . 24) ('velo . 0.6)) (('pos . 25) ('velo . 0.6)))
|#

(n note: (lambda(x) (* x 2) ) 
       (n type: 'note note: >> 61 62 63 ))

(+ 10 10 10)
(tra! 10 1/2
  (notation velo: << (cl 0.6 ) pos: >> (iota 16) ))

(add-incremental-keyword "hedd" "bars" )
  bars

    


(#|
   ========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========
   
   NAME: NOTATION
   
   SYNOPSIS: (notation|n [arg::any]...)::notation
   
   DESCRIPTION: notation creates a notation or a list of
   notations. TODO description
   
   ======================================================================
  |# help about-intro )


(make-help  '((names "notation" "n" ) 
              (params
                ("arg" "any" #f #t "see description "))
              (returns "::notation" )
              (short-description "<name/> creates a notation or a list of notations. " )
              (long-description  "TODO description" )))

(#|
   ========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========
   
   NAME: MAKE-HELP
   
   SYNOPSIS: (make-help content::(list cons ...))::void
   
   DESCRIPTION: ||make-help|| registers a reference manual for
   a procedure on the Pulsar documentation system. The
   ||content|| argument is the content of the reference manual. The value
   is an association list contains various data.
   
       '((name "foo-bar" "fb") 
         (params
            ("param-name" "param-type" "default-value" "#t if variable-length" "description") 
               ...
          )
         (returns "return-type" )
         (short-description "description" )
         (long-description  "description" )
       )
   
   The ||name|| field contains names of the procedure. In
   Pulsar, the most procedures have multiple names. The first element
   of this list is its 'long name' which should be the canonical
   name for the procedure. And the others are its aliases. If the
   procedure have no alias, then the list will have only one element.
   The list must have at least one element.
   
   The ||params|| field contains information of parameters.
   The field contains a list per a parameter.
   
   The ||short-description|| field contains a string value of
   its short description. The ||long-description|| field contains
   a string value of its long description.
   
   ======================================================================
  |# help about-intro )

(define (hello)
  'hello-world)

(hello)

(make-help
 hello
 '((names "hello" "hel" )
   (params
    ("par0" "number" "1" #f "great description")
    )
   (returns "::void")
   (short-description "foo" )
   (long-description "bar")))



(help hello)


                     



(#|
   ========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========
   
   NAME: GET-TRACK
   
   SYNOPSIS: (get-track [track-spec]...)::void
   
   DESCRIPTION: ||get-track|| retrieves multiple tracks which
   are specified as track-spec arguments. The tracks are stored
   in a linked list. See (help about-track-spec). In case the
   current sequencer system has not established any connection to the
   JACK, it throws an exception.
   ======================================================================
  |# help about-intro )

(if (not (open?))
  (begin
   (source "./ats's-drumkit.scm" )
   (source "./pulsar-pattern-generator.scm" )
   (create-gui)
   ))

(close)

((gnu.kawa.io.Path "."):absolute?)

(java.io.File (java.net.URI "a"))

(java.lang.System:setProperty "user.dir" "/home/ats/Documents/works/java/pulsar/workspace/pulsar/examples" )
(java.lang.System:setProperty "user.dir" "/home/ats/Documents/" )
(java.lang.System:getProperty "user.dir"  )

(define (y a b c) 
  gnu.mapping.CallContext:instance:proc) 
  )

(define (x a b)
  (y a b 3))

(apply x (list 1 1))

(((java.io.File "." ):getAbsoluteFile):getCanonicalFile)
((java.io.File "."):exists)

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

