;===============================================================================
;
; This scheme script file defines a number of instruments which I frequently
; use. This does not make sense for those people other than me but this also
; might be an example to understand how to access to your favorite instruments
; from Pulsar sequencer.
;
; See 
;    RHYTHM_PATTERNS
;    INSTRUMENTALS
; 
; (Tue, 30 Jul 2019 11:32:22 +0900)
;===============================================================================

(import (srfi 1))
(import (kawa pprint))

; n= a number of swing sequences ( c8 r8 c8 c8 ) which length usually equals to a halftone.
; ex (n-swing 2 1) generates the orthodox swing pattern which consists 4 quarter tones.
;
; Parameters 
;   n       : the number to repeat the specific pattern.
;   b       : the bar count duruing which the specific pattern to repeat.
;   ns-proc : specifying the pattern to repeat. 
(define n-swing 
  (lambda(n b ns-proc) 
    (cons 
      (len b)
      (sca! b
            (apply append 
                   (map 
                     (lambda (x)
                       (mov! (/ x n)
                             (sca! (/ 1 n)
                                   (ns-proc x n))))
                     (iota n )))))))

; ====================================================================================
; bind-pns
; This procedure binds a pnsprocedure with an instrument procedure and returns
; ns-proc. The term "pns" stands for plain n-swing.
(define bind-pns (lambda (inst pns-proc) 
                       (lambda (x n)
                         (pns-proc inst x n))))


;==============================================================================================
; INSTRUMENTALS
;==============================================================================================
(define key-name-value-list-to-name-value-list 
  (lambda ( key-name-value-list )
    (map 
      (lambda (e) (cons (cadr e) (cddr e))) 
      key-name-value-list )))

(define key-name-value-list-to-name-key-list 
  (lambda ( key-name-value-list )
    (map 
      (lambda (e) (cons (cadr e) (car e))) 
      key-name-value-list )))

(define insert-list-by-index 
  (lambda (dst-list src-list idx)
    (let-values (((head tail) (split-at dst-list idx )))
                (append head src-list tail))))
  
;
; This is association-list of symbols and cons-cells. The key consists symbols
; to identify each instrument. The value consists cons-cells. A cons-cell in
; the list contains instrument name on the car side, note name on the cdr side.

(define h2-inst-database 
  `(( inst-Hi-Hat-Free .           ( "Hi Hat Free" .               36 ))
    ( inst-Hi-Hat-Opened .         ( "Hi Hat Opened" .             37 ))
    ( inst-SB-HatBrushOpA1 .       ( "SB HatBrushOpA1" .           38 ))
    ( inst-SB-HatBrushChk .        ( "SB HatBrushChk" .            39 ))
    ( inst-SB-HatBrushClA1 .       ( "SB HatBrushClA1" .           40 ))
    ( inst-SB-HatBrushSplA1 .      ( "SB HatBrushSplA1" .          41 ))
    ( inst-Sabian-Hat-Semi-Open .  ( "Sabian Hat Semi-Open" .      42 ))
    ( inst-Sabian-Hat-Pedal .      ( "Sabian Hat Pedal" .          43 ))
    ( inst-Sabian-Hat-Swish .      ( "Sabian Hat Swish" .          44 ))
    ( inst-Sabian-Hat-Choke .      ( "Sabian Hat Choke" .          45 ))
    ( inst-Sabian-Hat-Open .       ( "Sabian Hat Open" .           46 ))
    ( inst-SB-Snr7BrusH .          ( "SB Snr7BrusH" .              47 ))
    ( inst-SB-Snr7BrusPA2 .        ( "SB Snr7BrusPA2" .            48 ))
    ( inst-SB-Snr7BrusPA3 .        ( "SB Snr7BrusPA3" .            49 ))
    ( inst-SB-Snr7BrusPA4 .        ( "SB Snr7BrusPA4" .            50 ))
    ( inst-SB-Snr7BrusPA5 .        ( "SB Snr7BrusPA5" .            51 ))
    ( inst-SB-Snr7BrusPA1 .        ( "SB Snr7BrusPA1" .            52 ))
    ( inst-SB-Snr7BrusPA6 .        ( "SB Snr7BrusPA6" .            53 ))
    ( inst-SB-Snr7BrusPA7 .        ( "SB Snr7BrusPA7" .            54 ))
    ( inst-Stick0 .                ( "Stick" .                     55 ))
    ( inst-Snare-Dry .             ( "Snare Dry" .                 56 ))
    ( inst-Snare1 .                ( "Snare1" .                    57 ))
    ( inst-Snare2 .                ( "Snare2" .                    58 ))
    ( inst-Snare-Jazz .            ( "Snare Jazz" .                59 ))
    ( inst-snare-2 .               ( "snare 2" .                   60 ))
    ( inst-snare-4 .               ( "snare 4" .                   61 ))
    ( inst-24-Snare-11 .           ( "24-Snare-11" .               62 ))
    ( inst-sn-606 .                ( "sn 606" .                    63 ))
    ( inst-sn-33 .                 ( "sn 33" .                     64 ))
    ( inst-sn-55 .                 ( "sn 55" .                     65 ))
    ( inst-909-clap .              ( "909 clap" .                  66 ))
    ( inst-Hand-Clap .             ( "Hand Clap" .                 67 ))
    ( inst-Kick .                  ( "Kick" .                      68 ))
    ( inst-Kick-Short .            ( "Kick Short" .                69 ))
    ( inst-Kick-Long .             ( "Kick Long" .                 70 ))
    ( inst-SB-Kik18openA1 .        ( "SB Kik18openA1" .            71 ))
    ( inst-SB-Kik22psoA1 .         ( "SB Kik22psoA1" .             72 ))
    ( inst-Ride .                  ( "Ride" .                      73 ))
    ( inst-Ride-Bell .             ( "Ride Bell" .                 74 ))
    ( inst-Ride-2 .                ( "Ride 2" .                    75 ))
    ( inst-Ride-Jazz .             ( "Ride Jazz" .                 76 ))
    ( inst-Ride-Rock .             ( "Ride Rock" .                 77 ))
    ( inst-SB-Ride1Brush .         ( "SB Ride1Brush" .             78 ))
    ( inst-SB-Ride2Brush .         ( "SB Ride2Brush" .             79 ))
    ( inst-SB-Crash2BA1 .          ( "SB Crash2BA1" .              80 ))
    ( inst-SB-Crash1BA1 .          ( "SB Crash1BA1" .              81 ))
    ( inst-Paiste-Ride .           ( "Paiste Ride" .               82 ))
    ( inst-Paiste-Ride-Flink .     ( "Paiste Ride Flink" .         83 ))
    ( inst-Paiste-Bell .           ( "Paiste Bell" .               84 ))
    ( inst-Sabian-Crash .          ( "Sabian Crash" .              85 ))
    ( inst-Sabian-Crash-Flink .    ( "Sabian Crash Flink" .        86 ))
    ( inst-Zildjian-Splash .       ( "Zildjian Splash" .           87 ))
    ( inst-Zildjian-Splash-Choke . ( "Zildjian Splash Choke" .     88 ))
    ( inst-Crash-Jazz .            ( "Crash Jazz" .                89 ))
    ( inst-Crash .                 ( "Crash" .                     90 ))
    ( inst-Sizzle-Cymbal .         ( "Sizzle Cymbal" .             91 ))
    ( inst-Crash-Cymbal .          ( "Crash Cymbal" .              92 ))
    ( inst-SB-Ride1BrushRoll2 .    ( "SB Ride1BrushRoll2" .        93 ))
    ( inst-SB-Ride1BrushRoll .     ( "SB Ride1BrushRoll" .         94 ))
    ( inst-Djembe1-Tone .          ( "Djembe1 Tone" .              95 ))
    ( inst-Djembe2-Slap .          ( "Djembe2 Slap" .              96 ))
    ( inst-Djembe2-Bass .          ( "Djembe2 Bass" .              97 ))
    ( inst-High-Tom .              ( "High Tom" .                  98 ))
    ( inst-Mid-Tom .               ( "Mid Tom" .                   99 ))
    ( inst-Floor-Tom .             ( "Floor Tom" .                100 ))
    ( inst-SB-TomB10in .           ( "SB TomB10in" .              101 ))
    ( inst-SB-TomB12in .           ( "SB TomB12in" .              102 ))
    ( inst-SB-TomB14in .           ( "SB TomB14in" .              103 ))
    ( inst-SB-TomB16in .           ( "SB TomB16in" .              104 ))
    ( inst-Cowbell .               ( "Cowbell" .                  105 ))
    ( inst-Kenkeni1-Bell-Hit .     ( "Kenkeni1 Bell Hit" .        106 ))
    ( inst-Kenkeni1-Bell-Mute .    ( "Kenkeni1 Bell Mute" .       107 ))
    ( inst-Dununba1-Bell .         ( "Dununba1 Bell" .            108 ))
    ( inst-Dununba1-Bell-Mute .    ( "Dununba1 Bell Mute" .       109 ))
))

(define inst-list (map (lambda (x)
                         (cons (car x) 
                               (cons (cadr x)
                                     (make-perc 0 0 (cddr x) 1/8 )
                                     )))
                       h2-inst-database ))

(define (sym2val s lst)
  (if (not (symbol? s))
    (raise (cons 'invalid-argument-exception s  ) ))
  (cdr (or (assq s lst)
           (raise (string-append "instrumental not found error "
                                 (symbol->string s))))))

; ADDED (Wed, 31 Jul 2019 19:53:22 +0900)
(define (counting-voice x)
  (make-perc 1 0 (+ C4  x)  1/4 ))

(define count-voices 
  (map 
    (lambda (x)
      (make-perc 1 0 (+ C4  x)  1/4 ))
    (iota 20)))

; ===========================================================================================
; RHYTHM_PATTERNS
; ===========================================================================================

; This is association-list of symbols and cons-cells. The key consists symbols
; to identify each pattern. The value consists cons-cells. A cons-cell in the
; list contains instrument name on the car side, a procedure object to generate
; pattern on the cdr side.
;
; We call the procedure "pns-function".
(define pns-list
  `((pns-two-four         .  ( "2-4 Beat"    . ,(lambda (inst0 x n) 
                                                  (list
                                                    (inst0 'A           #t (/ 2 4) (+ 0.5  (rnd -0.0  0.1 )))))) )

    (pns-basic-one        .  ( "1 Beat"      . ,(lambda (inst0 x n) 
                                                  (list
                                                    (inst0 'A (luck 1.00 ) 0 (+ 0.3  (rnd -0.0  0.1 )))))) )

    (pns-basic-4-swing    .  ( "4-Swing"     . ,(lambda (inst0 x n) 
                                                  (list
                                                    (inst0 'A (luck 1.00 ) (/ 0 4) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 0.00 ) (/ 1 4) (+ 0.5  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 1.00 ) (/ 2 4) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 1.00 ) (/ 3 4) (+ 0.5  (rnd -0.0  0.3 )))))) )

    (pns-basic-6-swing    .  ( "6-Swing"     . ,(lambda (inst0 x n) 
                                                  (list
                                                    (inst0 'A (luck 1.00 ) (/ 0 6) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 0.00 ) (/ 2 6) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 1.00 ) (/ 3 6) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 1.00 ) (/ 5 6) (+ 0.5  (rnd -0.0  0.3 ))))))) 

    (pns-basic-5-swing    .  ( "5-Swing"     . ,(lambda (inst0 x n) 
                                                  (list
                                                    (inst0 'A (luck 1.00 ) (/ 0 5) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 0.00 ) (/ 1 5) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 1.00 ) (/ 2 5) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 0.00 ) (/ 3 5) (+ 0.3  (rnd -0.0  0.1 )))
                                                    (inst0 'A (luck 1.00 ) (/ 4 5) (+ 0.5  (rnd -0.0  0.3 )))))) )

    (pns-ntime            .  ( "pns-ntime"   .          ,(lambda (inst0 x n) 
                                                           (list
                                                             (inst0 'A (luck 1.00 ) (/ 0 4) (+ 0.3  (rnd -0.0  0.1 )))))) )

    (pns-counting         .  ( "Count Beat"  . ,(lambda (dummy-inst x n)
                                                  (list
                                                    (let ((cnt (list-ref count-voices x)))
                                                      (cnt '(A) #t 0 (rnd 0.5 0.7 )))))))

    (create-count-measure . ( "Count Bar"    .  ,(lambda (nm)
                                                   (append
                                                     (map
                                                       (lambda (n)
                                                         (let ((cnt (list-ref count-voices n)))
                                                           (cnt '(A) #t (/ n nm ) (rnd 0.5 0.7 ))))
                                                       (iota nm))
                                                     (list (len 1))))))))


(define main-track-sequence (lambda () (list (len 4/4))))
(set-main   (lambda()
               (display-warn "====set-main SET-MAIN ========\n" )
               (newline-warn)
               (put-track  (new-track 'main (lambda args
                                              (apply main-track-sequence args ))))))




(if (not (open?))
  (begin 
    ;(close)
    (open     "pulsar" )
    (open-output   "out-h2" "out-counter" "out-fluidsynth" )
    (open-input    "MIDI Input0"  "MIDI Input1"  )
    (set-tempo 120)

    (connect  "pulsar:out-h2"         "hydrogen-midi:RX" )
    (connect  "pulsar:out-counter"    "qsynth-counter:midi" )
    (connect  "pulsar:out-fluidsynth" "qsynth-fluidsynth:midi" )

    (rewind)))


; vim: filetype=scheme expandtab :
