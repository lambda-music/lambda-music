; 
; Pulsar-Sequencer written by Atsushi Oka 
; Copyright 2018 Atsushi Oka
; 
; This file is part of Pulsar-Sequencer. 
; 
; Pulsar-Sequencer is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; Pulsar-Sequencer is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
; 
(display "Welcome to pulsar a drum machine!")
(newline)

    (define G9  127	)
    (define Gb9 126	)
    (define F9  125	)
    (define E9  124	)
    (define Eb9 123	)
    (define D9  122	)
    (define Db9 121	)
    (define C9  120	)
    (define B8  119	)
    (define Bb8 118	)
    (define A8  117	)
    (define Ab8 116	)
    (define G8  115	)
    (define Gb8 114	)
    (define F8  113	)
    (define E8  112	)
    (define Eb8 111	)
    (define D8  110	)
    (define Db8 109	)
    (define C8  108	)
    (define B7  107	)
    (define Bb7 106	)
    (define A7  105	)
    (define Ab7 104	)
    (define G7  103	)
    (define Gb7 102	)
    (define F7  101	)
    (define E7  100	)
    (define Eb7 99	)
    (define D7  98	)
    (define Db7 97	)
    (define C7  96	)
    (define B6  95	)
    (define Bb6 94	)
    (define A6  93	)
    (define Ab6 92	)
    (define G6  91	)
    (define Gb6 90	)
    (define F6  89	)
    (define E6  88	)
    (define Eb6 87	)
    (define D6  86	)
    (define Db6 85	)
    (define C6  84	)
    (define B5  83	)
    (define Bb5 82	)
    (define A5  81	)
    (define Ab5 80	)
    (define G5  79	)
    (define Gb5 78	)
    (define F5  77	)
    (define E5  76	)
    (define Eb5 75	)
    (define D5  74	)
    (define Db5 73	)
    (define C5  72	)
    (define B4  71	)
    (define Bb4 70	)
    (define A4  69	)
    (define Ab4 68	)
    (define G4  67	)
    (define Gb4 66	)
    (define F4  65	)
    (define E4  64	)
    (define Eb4 63	)
    (define D4  62	)
    (define Db4 61	)
    (define C4  60	)
    (define B3  59	)
    (define Bb3 58	)
    (define A3  57	)
    (define Ab3 56	)
    (define G3  55	)
    (define Gb3 54	)
    (define F3  53	)
    (define E3  52	)
    (define Eb3 51	)
    (define D3  50	)
    (define Db3 49	)
    (define C3  48	)
    (define B2  47	)
    (define Bb2 46	)
    (define A2  45	)
    (define Ab2 44	)
    (define G2  43	)
    (define Gb2 42	)
    (define F2  41	)
    (define E2  40	)
    (define Eb2 39	)
    (define D2  38	)
    (define Db2 37	)
    (define C2  36	)
    (define B1  35	)
    (define Bb1 34	)
    (define A1  33	)
    (define Ab1 32	)
    (define G1  31	)
    (define Gb1 30	)
    (define F1  29	)
    (define E1  28	)
    (define Eb1 27	)
    (define D1  26	)
    (define Db1 25	)
    (define C1  24	)
    (define B0  23	)
    (define Bb0 22	)
    (define A0  21	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define sol9      G9  )
    (define se9     Gb9 )
    (define fi9     Gb9 )
    (define fa9      F9  )
    (define mi9      E9  )
    (define me9     Eb9 )
    (define ri9     Eb9 )
    (define re9      D9  )
    (define ra9     Db9 )
    (define di9     Db9 )
    (define do9      C9  )
    (define ti8      B8  )
    (define te8     Bb8 )
    (define li8     Bb8 )
    (define la8      A8  )
    (define le8     Ab8 )
    (define si8     Ab8 )
    (define sol8      G8  )
    (define se8     Gb8 )
    (define fi8     Gb8 )
    (define fa8      F8  )
    (define mi8      E8  )
    (define me8     Eb8 )
    (define ri8     Eb8 )
    (define re8      D8  )
    (define ra8     Db8 )
    (define di8     Db8 )
    (define do8      C8  )
    (define ti7      B7  )
    (define te7     Bb7 )
    (define li7     Bb7 )
    (define la7      A7  )
    (define le7     Ab7 )
    (define si7     Ab7 )
    (define sol7      G7  )
    (define se7     Gb7 )
    (define fi7     Gb7 )
    (define fa7      F7  )
    (define mi7      E7  )
    (define me7     Eb7 )
    (define ri7     Eb7 )
    (define re7      D7  )
    (define ra7     Db7 )
    (define di7     Db7 )
    (define do7      C7  )
    (define ti6      B6  )
    (define te6     Bb6 )
    (define li6     Bb6 )
    (define la6      A6  )
    (define le6     Ab6 )
    (define si6     Ab6 )
    (define sol6      G6  )
    (define se6     Gb6 )
    (define fi6     Gb6 )
    (define fa6      F6  )
    (define mi6      E6  )
    (define me6     Eb6 )
    (define ri6     Eb6 )
    (define re6      D6  )
    (define ra6     Db6 )
    (define di6     Db6 )
    (define do6      C6  )
    (define ti5      B5  )
    (define te5     Bb5 )
    (define li5     Bb5 )
    (define la5      A5  )
    (define le5     Ab5 )
    (define si5     Ab5 )
    (define sol5      G5  )
    (define se5     Gb5 )
    (define fi5     Gb5 )
    (define fa5      F5  )
    (define mi5      E5  )
    (define me5     Eb5 )
    (define ri5     Eb5 )
    (define re5      D5  )
    (define ra5     Db5 )
    (define di5     Db5 )
    (define do5      C5  )
    (define ti4      B4  )
    (define te4     Bb4 )
    (define li4     Bb4 )
    (define la4      A4  )
    (define le4     Ab4 )
    (define si4     Ab4 )
    (define sol4      G4  )
    (define se4     Gb4 )
    (define fi4     Gb4 )
    (define fa4      F4  )
    (define mi4      E4  )
    (define me4     Eb4 )
    (define ri4     Eb4 )
    (define re4      D4  )
    (define ra4     Db4 )
    (define di4     Db4 )
    (define do4      C4  )
    (define ti3      B3  )
    (define te3     Bb3 )
    (define li3     Bb3 )
    (define la3      A3  )
    (define le3     Ab3 )
    (define si3     Ab3 )
    (define sol3      G3  )
    (define se3     Gb3 )
    (define fi3     Gb3 )
    (define fa3      F3  )
    (define mi3      E3  )
    (define me3     Eb3 )
    (define ri3     Eb3 )
    (define re3      D3  )
    (define ra3     Db3 )
    (define di3     Db3 )
    (define do3      C3  )
    (define ti2      B2  )
    (define te2     Bb2 )
    (define li2     Bb2 )
    (define la2      A2  )
    (define le2     Ab2 )
    (define si2     Ab2 )
    (define sol2      G2  )
    (define se2     Gb2 )
    (define fi2     Gb2 )
    (define fa2      F2  )
    (define mi2      E2  )
    (define me2     Eb2 )
    (define ri2     Eb2 )
    (define re2      D2  )
    (define ra2     Db2 )
    (define di2     Db2 )
    (define do2      C2  )
    (define ti1      B1  )
    (define te1     Bb1 )
    (define li1     Bb1 )
    (define la1      A1  )
    (define le1     Ab1 )
    (define si1     Ab1 )
    (define sol1      G1  )
    (define se1     Gb1 )
    (define fi1     Gb1 )
    (define fa1      F1  )
    (define mi1      E1  )
    (define me1     Eb1 )
    (define ri1     Eb1 )
    (define re1      D1  )
    (define ra1     Db1 )
    (define di1     Db1 )
    (define do1      C1  )
    (define ti0      B0  )
    (define te0     Bb0 )
    (define li0     Bb0 )
    (define la0      A0  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








; vim: filetype=scheme expandtab :
