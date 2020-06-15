Lamu Notation Reference
======================

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (n type: '[no-operation|nop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (n type: '[note-on-off|note] [XXX|port]: string|number=0 [XXX|chan]: number=0 [XXX|pos]: number=0.0 [XXX|note]: number=60(C4) [XXX|velo]: number=0.5 [XXX|len]: number=0.0025d
)

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (n type: '[length|len] [XXX|val]: number=0.0
)

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (n type: '[execute|exec] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

EXECUTE-TRACK
====================

#### SYNOPSIS ####
    (n type: '[execute-track|exet] [XXX|val]: number=0.0
)

### DESCRIPTION ###
execute-track executes the specific track-manipulator. execute-track executes
the specific track-manipulator.



--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (n type: '[end-track|end] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (n type: '[note on|non] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (n type: '[note off|noff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[key-pressure|kp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (n type: '[control-change|cc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (n type: '[program|pc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (n type: '[channel-pressure|cp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (n type: '[pitch-bend|pb] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-sound-off|aso] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (n type: '[reset-all-controllers|rac] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (n type: '[local-controls|lc] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (n type: '[all-note-off|anf] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-off|omff] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[omni-mode-on|omon] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[mono-mode-on|mono] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (n type: '[poly-mode-on|poly] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (n type: '[sys-song-position-pointer|spp] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (n type: '[sys-song-select|ss] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (n type: '[sys-end-of-exclusive|eoe] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (n type: '[sys-clock|clock] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (n type: '[sys-start|start] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (n type: '[sys-continue|cont] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (n type: '[sys-stop|stop] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (n type: '[sys-reset|reset] 
)

### DESCRIPTION ###
 



--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (n type: '[bank-select|bs] 
)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (n type: '[modulation|mod] 
)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect \(pitch, loudness,
brighness\). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[breath-controller|bc] 
)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (n type: '[foot-controller|fc] 
)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (n type: '[portamento-time|pt] 
)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (n type: '[data-entry-msb|de-msb] 
)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit\(MSB\) Controls Value for NRPN or
RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (n type: '[volume|v] 
)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (n type: '[balance|b] 
)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (n type: '[pan|p] 
)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (n type: '[expression|e] 
)

### DESCRIPTION ###
Expression Expression is a percentage of volume \(CC7\).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-1|ec1] 
)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[effect-controller-2|ec2] 
)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (n type: '[sustain-pedal|sp] 
)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. \(See also
Sostenuto CC 66\)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[portamento-switch|ps] 
)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[sostenuto-switch|sos-s] 
)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch - Like the Sustain controller \(CC 64\),
However it only holds notes that were "On" when the pedal was pressed. People use it to
"hold" chords" and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[soft-pedal-switch|soft-pedal] 
)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (n type: '[legato-switch|ls] 
)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (n type: '[hold-2|h2] 
)

### DESCRIPTION ###
Hold 2 Another way to "hold notes" \(see MIDI CC 64 and MIDI CC 66\). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-1|sc1] 
)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-2|sc2] 
)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter \(VCF\).
Default = Resonance -also\(Timbre or Harmonics\)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-3|sc3] 
)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
\(VCA\). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-4|sc4] 
)

### DESCRIPTION ###
Sound Controller 4 Controls the "Attack" of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-5|sc5] 
)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-6|sc6] 
)

### DESCRIPTION ###
Sound Controller 6 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-7|sc7] 
)

### DESCRIPTION ###
Sound Controller 7 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-8|sc8] 
)

### DESCRIPTION ###
Sound Controller 8 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-9|sc9] 
)

### DESCRIPTION ###
Sound Controller 9 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (n type: '[sound-controller-10|sc10] 
)

### DESCRIPTION ###
Sound Controller 10 Generic - Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-01|gp01] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-02|gp02] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-03|gp03] 
)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (n type: '[general-purpose-cc-04|gp04] 
)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (n type: '[portamento|po] 
)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (n type: '[effect-1|e1] 
)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (n type: '[effect-2|e2] 
)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (n type: '[effect-3|e3] 
)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (n type: '[effect-4|e4] 
)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (n type: '[effect-5|e5] 
)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-increment|inc] 
)

### DESCRIPTION ###
\(+1\) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (n type: '[data-decrement|dec] 
)

### DESCRIPTION ###
\(-1\) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-lsb|nrpn-l] 
)

### DESCRIPTION ###
Non-Registered Parameter Number LSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[nrpn-msb|nrpn-m] 
)

### DESCRIPTION ###
Non-Registered Parameter Number MSB \(NRPN\) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-lsb|rpn-l] 
)

### DESCRIPTION ###
Registered Parameter Number LSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (n type: '[rpn-msb|rpn-m] 
)

### DESCRIPTION ###
Registered Parameter Number MSB \(RPN\) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

