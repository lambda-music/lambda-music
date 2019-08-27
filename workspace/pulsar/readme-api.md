OPEN?
====================

#### SYNOPSIS ####
    (open?)::boolean

### DESCRIPTION ###
returns the current open state. This procedure returns #t iff the current
sequencer state is open; otherwise returns #f.



--------------------------------------------------------

OPEN
====================

#### SYNOPSIS ####
    (open client-name::string)::void

### DESCRIPTION ###
starts a new connection between JACK Audio Connection Kit. This procedure opens
a new connection to the installed JACK Audio Connection Kit withthe specified
client name. When it failed to open a connection, this throws an exception. This
procedure alters the current sequencer system's state.



--------------------------------------------------------

CLOSE
====================

#### SYNOPSIS ####
    (close)::void

### DESCRIPTION ###
ends the current connection between JACK Audio Connection Kit. This procedure
closes the current connection to the JACK Audio Connection Kit. When it failed to
close the connection, this throws an exception. In case the current sequencer system
has not established any connection to the JACK, it throws an exception. This
procedure alters the current sequencer system's state.



--------------------------------------------------------

OPEN-OUTPUT
====================

#### SYNOPSIS ####
    (open-output|openo [port-name::any|(list any ...)]...)::MetroPort

### DESCRIPTION ###
opens output ports on the current JACK connection. Each argument is the name
of a port to create. The value can be a value of any type; thought, it is usually
a value which is easy to be distinguished such as a symbol value or a string
value. The value is applied as the identifier of the created port. A duplicated port
name on the current JACK connection causes an exception to be thrown. In case the
current sequencer system has not established any connection to the JACK, it throws an
exception. This procedure alters the current sequencer system's state.



--------------------------------------------------------

OPEN-INPUT
====================

#### SYNOPSIS ####
    (open-input|openi [port-name::any|(list any ...)]...)::MetroPort

### DESCRIPTION ###
opens input ports on the current JACK connection. Each argument is the name of
a port to create. The value can be a value of any type; thought, it is usually a
value which is easy to be distinguished such as a symbol value or a string value. The
value is applied as the identifier of the created port. A duplicated port name on the
current JACK connection causes an exception to be thrown. In case the current sequencer
system has not established any connection to the JACK, it throws an exception. This
procedure alters the current sequencer system's state.



--------------------------------------------------------

CLOSE-OUTPUT
====================

#### SYNOPSIS ####
    (close-output|closeo [port::MetroPort|symbol|string|(list MetroPort|symbol|string ...)]...)::void

### DESCRIPTION ###
closes the specified output ports on the current JACK connection. Each
argument is a reference to a MetroPort object to close. A value which is other than a
reference to a MetroPort object is treated as an identifier of a MetroPort object and
automatically replaced with a reference value to the corresponding MetroPort object. The
value is applied as the identifier of the created port. A duplicated port name on the
current JACK connection causes an exception to be thrown. In case the current sequencer
system has not established any connection to the JACK, it throws an exception. This
procedure alters the current sequencer system's state.



--------------------------------------------------------

CLOSE-INPUT
====================

#### SYNOPSIS ####
    (close-input|closei [port::MetroPort|symbol|string|(list MetroPort|symbol|string ...)]...)::void

### DESCRIPTION ###
closes the specified input ports on the current JACK connection. Each argument
is a reference to a MetroPort object to close. A value which is other than a
reference to a MetroPort object is treated as an identifier of a MetroPort object and
automatically replaced with a reference value to the corresponding MetroPort object. The
value is applied as the identifier of the created port. A duplicated port name on the
current JACK connection causes an exception to be thrown. In case the current sequencer
system has not established any connection to the JACK, it throws an exception. This
procedure alters the current sequencer system's state.



--------------------------------------------------------

LIST-OUTPUT
====================

#### SYNOPSIS ####
    (list-output|lso)::(list MetroPort ...)

### DESCRIPTION ###
returns a list which contains all output ports on the current JACK connection.
Each element on the list is a reference to a MetroPort object. The values in the
list are sorted from newest to oldest. In case the current sequencer system has not
established any connection to the JACK, it throws an exception.



--------------------------------------------------------

LIST-INPUT
====================

#### SYNOPSIS ####
    (list-input|lsi)::(list MetroPort ...)

### DESCRIPTION ###
returns a list which contains all input ports on the current JACK connection.
Each element on the list is a reference to a MetroPort object. The values in the
list are sorted from newest to oldest. In case the current sequencer system has not
established any connection to the JACK, it throws an exception.



--------------------------------------------------------

CONNECT
====================

#### SYNOPSIS ####
    (connect [from::string]... [to::string]...)::void

### DESCRIPTION ###
connects specified two ports on the current JACK connection. This procedure
connects the port which is specified in the first argument to the port which is
specified in the second argument. The rest arguments are also processed in the same
manner; that is this procedure connects each port in the argument which position is in
odd ordinal number, to the port in the argument which position is in even ordinal
number.

A canonical port name consists two parts; these are separated by a semicolon
and the former part is the name of a client and the latter is the name of a port as
"a-nice-client:the-port".

It is able to enumerate all ports by ||get-all-output|| and ||get-all-input||
procedure. In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

DISCONNECT
====================

#### SYNOPSIS ####
    (disconnect [from::string]... [to::string]...)::void

### DESCRIPTION ###
disconnects specified two ports on the current JACK connection. This procedure
disconnects the port which is specified in the first argument to the port which is
specified in the second argument. The rest arguments are also processed in the same
manner; that is this procedure disconnects each port in the argument which position is
in odd ordinal number, to the port in the argument which position is in even
ordinal number.

A canonical port name consists two parts; these are separated by a semicolon
and the former part is the name of a client and the latter is the name of a port as
"a-nice-client:the-port".

It is able to enumerate all ports by ||get-all-output|| and ||get-all-input||
procedure. In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

GET-ALL-OUTPUT
====================

#### SYNOPSIS ####
    (get-all-output|gao)::list<string>

### DESCRIPTION ###
retrieves IDs of all output connections in the current session of JACK Audio
Connection Kit. Each ID contains two parts which are separated by a separator character
":"; the former part is the server name part and the latter part is the port name
part.The passed arguments are silently discarded. In case the current sequencer system
has not established any connection to the JACK, it throws an exception.



--------------------------------------------------------

GET-ALL-INPUT
====================

#### SYNOPSIS ####
    (get-all-input|gai)::list<string>

### DESCRIPTION ###
retrieves IDs of all input connections in the current session of JACK Audio
Connection Kit. Each ID contains two parts which are separated by a separator character
":"; the former part is the server name part and the latter part is the port name
part.The passed arguments are silently discarded. In case the current sequencer system
has not established any connection to the JACK, it throws an exception.



--------------------------------------------------------

SET-MAIN
====================

#### SYNOPSIS ####
    (set-main main-procedure::procedure)::void

### DESCRIPTION ###
sets the main procedure. The main procedure is a procedure which is called
when (rewind) procedure is called in order to reset the sequencer's state. Usually,
the main procedure is a procedure to boot up the current song system.



--------------------------------------------------------

GET-MAIN
====================

#### SYNOPSIS ####
    (get-main)::procedure

### DESCRIPTION ###
retrieves the main procedure. See (help set-main) for further information.



--------------------------------------------------------

SET-PLAYING
====================

#### SYNOPSIS ####
    (set-playing playing::boolean)::void

### DESCRIPTION ###
sets the current playing state. When #f is passed to this procedure, the
sequencer stops playing. When #t is passed to this procedure ,the sequencer resumes
playing. In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

PLAYING?
====================

#### SYNOPSIS ####
    (playing?)::boolean

### DESCRIPTION ###
retrieves the current playing state. When the sequencer is playing, it returns
#t; otherwise it returns #f. See (help set-playing) for further information. In
case the current sequencer system has not established any connection to the JACK, it
throws an exception.



--------------------------------------------------------

PLAY
====================

#### SYNOPSIS ####
    (play)::void

### DESCRIPTION ###
causes the sequencer to start playing. See (help set-play) for further
information.In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

STOP
====================

#### SYNOPSIS ####
    (stop)::void

### DESCRIPTION ###
causes the sequencer to stop playing. See (help set-play) for further
information.In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

QUIT
====================

#### SYNOPSIS ####
    (quit)::void

### DESCRIPTION ###
quits the application. makes the sequencer to stop playing and shutdowns the
application in 1024 milliseconds. Currently the time to shutdown is hard-coded and cannot
be changed. In case the current sequencer system has not established any
connection to the JACK, it throws an exception.



--------------------------------------------------------

TAP-TEMPO
====================

#### SYNOPSIS ####
    (tap-tempo|tapt)::void

### DESCRIPTION ###
has the same effect with pressing the tap-tempo button on the main screen. The
tap-tempo button is a button to change the current tempo. The button is supposed to be
pressed repeatedly to tell the system how fast the sequencer should play the current
music. The sequencer calculates the average of durations between the pressing the
button, and apply the value as the current tempo on the sequencer system. See (help
set-tempo).In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

SET-TEMPO
====================

#### SYNOPSIS ####
    (set-tempo tempo::number)::void

### DESCRIPTION ###
sets the current tempo. This procedure takes an argument as a
beat-per-minutes. A value less than one is treated as one. There is no maximum value for the
argument. thought, the result of passing extreme values to the procedure is not defined.

See (help tap-tempo) for further information.In case the current sequencer
system has not established any connection to the JACK, it throws an exception.



--------------------------------------------------------

RESET
====================

#### SYNOPSIS ####
    (reset)::void

### DESCRIPTION ###
resets the environment object of Scheme interpreter, and close the current JACK
connection. This procedure is supposed to be called interactively and is not supposed to be
called from other procedures; a procedure which called the (reset) procedure will be
deleted from the current environment object as well as other procedures and as a
result, the procedure cannot call other procedures which are necessary to continue the
process.



--------------------------------------------------------

REWIND
====================

#### SYNOPSIS ####
    (rewind)::void

### DESCRIPTION ###
causes the music sequencer to go to the head of the song. This procedure
usually causes the music sequencer to call the main-procedure. See (help set-main). In
case the current sequencer system has not established any connection to the JACK, it
throws an exception.



--------------------------------------------------------

SIMULTANEOUS
====================

#### SYNOPSIS ####
    (simultaneous|simul [subproc::procedure]...)::void

### DESCRIPTION ###
executes passed the procedures "simultaneously". This procedure is designed to
add multiple tracks to the sequencer and and let the tracks start to play
simultaneously. While the interpreter is executing this procedure, the thread that is
processing tracks in order to generate the music data is blocked. Therefore, it is
guaranteed that the sequencer starts to process the added tracks simultaneously. In case
the current sequencer system has not established any connection to the JACK, it
throws an exception.



--------------------------------------------------------

GET-TRACK
====================

#### SYNOPSIS ####
    (get-track|gett [track-spec::any]...)::void

### DESCRIPTION ###
||get-track|| retrieves multiple tracks which are specified as track-spec
arguments. The tracks are stored in a linked list. See (help about-track-spec). In case
the current sequencer system has not established any connection to the JACK, it
throws an exception.



--------------------------------------------------------

ABOUT-TRACK-SPEC
====================

#### SYNOPSIS ####
    (about-track-spec)

### DESCRIPTION ###
The track-spec denotes a specification of a track to retrieve. Only symbol,
string and procedure are valid as a track-spec.

When track-spec is a symbol/a string, the value is compared with the name
value of each track, and the track is added to the result when it equals to the
value. It uses the equals() method of java.lang.Object class to check the equality of
the two values.

When track-spec is a procedure: The system enumerates all tracks in the current
sequencer, and call the specified procedure for each track. The procedure should have two
parameters : (lambda ( name tags ) ... ). If a track identified by the name and the tags
is not to retrieve, the procedure should return #f; otherwise the track is
selected to the result.

In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

NEW-TRACK
====================

#### SYNOPSIS ####
    (new-track|newt [notations::procedure/(list notation)]...)::MetroTrack

### DESCRIPTION ###
new-track creates a new track. A track is a basic unit of music in Pulsar music
sequencer. A track contains a procedure to create a notation list. When a user added a
track to the sequencer, the sequencer asks what to play next to the track. The
sequencer plays it and asks to the track again when it finished to play the notation
list. The length of a notation list which a track creates is usually one measure; but
it can be any length. The sequencer can have multiple tracks. There is no limit on
maximum number of tracks. It is necessary to add the track which is created by
new-track procedure to the sequencer by (put-track) procedure. See (help put-track) for
further information. In case the current sequencer system has not established any
connection to the JACK, it throws an exception.



--------------------------------------------------------

ABOUT-NOTATION
====================

#### SYNOPSIS ####
    (about-notation)::void

### DESCRIPTION ###
A notation is a MIDI data which Pulsar music sequencer can play. In Pulsar, a
notation is made of a Scheme association list. There are several types of a notation
such as notes, rests, MIDI control changes and others. The contents of a notation
depend on its type; for example, if a notation is a note data, the notation object
have four properties : velocity, length, position and pitch. In case the current
sequencer system has not established any connection to the JACK, it throws an exception.



--------------------------------------------------------

ABOUT-INTRO
====================

#### SYNOPSIS ####
    (about-intro)

### DESCRIPTION ###
Welcome to Pulsar music sequencer! Pulsar music sequencer is a music sequencer
which collaboratively works with a powerful computer language Lisp Scheme. And this
frame itself is a powerful Lisp Scheme editor which is called KawaPad. In Lisp, all
commands are surrounded with a pair of parentheses. You can easily execute one of those
command by moving your cursor within the pair of parentheses and pressing CTRL+ENTER.

To show this help, execute (help about-intro). To show all available
procedures, execute (help) . To show help of a procedure, execute (help [procedure-name] )
.



--------------------------------------------------------

PUT-TRACK
====================

#### SYNOPSIS ####
    (put-track|putt [sync-type::symbol=] [sync-track::MetroTrack|track-spec=] [sync-offset::number=])

### DESCRIPTION ###
put the passed track on the sequencer. The sequencer starts to play the added
track and it gives the user some controls on how it starts playing the track.

The ||track|| parameter is the reference to the track which is to play.

The sync-type parameter can be one of ||immediate||, ||parallel|| and
||serial||.

When sync-type is ||immediate||, the sequencer starts to play the track as soon
as possible after returning from the procedure call. When sync-type is
||parallel||, the sequencer starts to play the track at the same position with the track
which is specified as ||sync-track|| parameter.

When sync-type is ||serial||, the sequencer starts to play the track right
after the track which is specified in the ||sync-track|| finished to play.

The sync-track parameter is the reference to the track which is to synchronize
with.

The sync-offset parameter is the time offset from the time that track is
supposed to start playing. The number must be a real number. It denotes the offset
length which unit is a measure-length.



--------------------------------------------------------

REMOVE-TRACK
====================

#### SYNOPSIS ####
    (remove-track|remt [sync-type::symbol=] [sync-track::MetroTrack|track-spec=] [sync-offset::number=])

### DESCRIPTION ###
removes the passed track on the sequencer. The sequencer remove the specified
track. Eventually the track stops playing. And it gives the user some controls on how
it stops playing the track.

The ||track|| parameter is the reference to the track which is to play.

The sync-type parameter can be one of ||immediate||, ||parallel|| and
||serial||.

When sync-type is ||immediate||, the sequencer starts to play the track as soon
as possible after returning from the procedure call. When sync-type is
||parallel||, the sequencer starts to play the track at the same position with the track
which is specified as ||sync-track|| parameter.

When sync-type is ||serial||, the sequencer starts to play the track right
after the track which is specified in the ||sync-track|| finished to play.

The sync-track parameter is the reference to the track which is to synchronize
with.

The sync-offset parameter is the time offset from the time that track is
supposed to start playing. The number must be a real number. It denotes the offset
length which unit is a measure-length.



--------------------------------------------------------

NOTIFY-TRACK-CHANGE
====================

#### SYNOPSIS ####
    (notify-track-change|nott)::void

### DESCRIPTION ###
notifies the sequencer that the track was added/deleted. When any tracks are
added/deleted on the sequencer, the modification is not immediately reflects to the current
state of the sequencer. After a series of adding/deleting tracks is performed by a
user,the the user is mandated to call this procedure. This procedure notifies the
sequencer that some tracks. And calling this procedure guarantees the tracks
added/deleted on the sequencer are properly processed immediately.



--------------------------------------------------------

LIST-TRACKS
====================

#### SYNOPSIS ####
    (list-tracks|lstt)::(list track ...)

### DESCRIPTION ###
||list-tracks|| retrieves all tracks on the current sequencer. The order of
the tracks in the result of this procedure follows the first-in-last-out
manner. That is, (car (list-tracks)) always returns the last added track.



--------------------------------------------------------

CLEAR-TRACKS
====================

#### SYNOPSIS ####
    (clear-tracks|clet)::void

### DESCRIPTION ###
||clear-tracks|| removes all tracks on the current sequencer immediately.



--------------------------------------------------------

PRINT-STACK-TRACE
====================

#### SYNOPSIS ####
    (print-stack-trace)::void

### DESCRIPTION ###
||print-stack-trace|| returns the current stack trace as a string.



--------------------------------------------------------

DISPLAY-WARN
====================

#### SYNOPSIS ####
    (display-warn value::any)::void

### DESCRIPTION ###
||display-warn|| output the specified value to the standard error stream.



--------------------------------------------------------

NEWLINE-WARN
====================

#### SYNOPSIS ####
    (newline-warn)::void

### DESCRIPTION ###
||newline-warn|| output a line terminator to the standard error stream.



--------------------------------------------------------

TYPEOF
====================

#### SYNOPSIS ####
    (typeof value::any)::string

### DESCRIPTION ###
||typeof|| returns a Java class name of the specified value. In case the
specified value is a ||null|| of Java, this procedure returns "null" as a string value.



--------------------------------------------------------

MAKE-TIMER
====================

#### SYNOPSIS ####
    (make-timer delay::number interval::number callback::procedure)::procedure

### DESCRIPTION ###
||make-timer|| creates a new timer object. This procedure registers the
specified procedure as a callback procedure of the timer; the procedure will be called
with the specified period and with the specified delay. The return value is a cancel
procedure. When the cancel procedure is called, the timer stops calling the callback
procedure.



--------------------------------------------------------

RANDOM
====================

#### SYNOPSIS ####
    (random|rnd [range::number=1])::number

### DESCRIPTION ###
||random|| generates a random number. This procedure adopts Mersenne Twister a
random number generating algorithm. If an argument [range] is specified, the return
value will be within 0<= x <[range]. If the argument is omitted, the range value
defaults to 1.



--------------------------------------------------------

LUCK
====================

#### SYNOPSIS ####
    (luck [probability::number=0.5])::boolean

### DESCRIPTION ###
||luck|| is a procedure that returns a random boolean value. The first
argument is the value of probability where the larger value causes the more probability
of returning #t. When the specified value is equals or less than zero, the
returning value is always #f. When the specified value is equals or larger than one the
returning value is always #t. The only parameter can be omitted and in that case the
default value one is applied.



--------------------------------------------------------

MAKE-PAGE
====================

#### SYNOPSIS ####
    (make-page content::string)::kawapad-page

### DESCRIPTION ###
make-page makes the passed value into ||kawapad-page|| object. When an
expression is evaluated in KawaPad, the result value is displayed on the current editor.
When the result value is a ||kawapad-page|| object, the value is displayed in a
special way; when the KawaPad system detect the result value is a ||kawapad-page||, the
editor expands the current selection to the outer-most parentheses and replace the
region with the result value. This enables it to use KawaPad as a dynamic Hypertext
editor.

The make-page procedure convert the passed value into the kawapad-page object
in order to activate the special display function of KawaPad. In case the current
sequencer system has not established any connection to the JACK, it throws an exception.



--------------------------------------------------------

HELP!
====================

#### SYNOPSIS ####
    (help!)::string

### DESCRIPTION ###
is a procedure to execute when the user needs something which calms you down.
When this procedure is called, this procedure will return a message which tries to
calm the user down. Any argument specified to this procedure will be silently
ignored.This procedure is deliberately defined as a joke and has by no means effect to the
current system state nor any other related elements. See (help about-main).



--------------------------------------------------------

HELP
====================

#### SYNOPSIS ####
    (help|he)::string|list

### DESCRIPTION ###
is a procedure to show the description of a specified procedure. When a
procedure reference is passed, it returns a string value that contains the description of
the the procedure.

If no procedure is specified, it returns a list that contains all procedures
which description is available. Pass a special keyword 'all-available to get a symbol
list of all procedures which are available for this command.



--------------------------------------------------------

HELP-MARKDOWN
====================

#### SYNOPSIS ####
    (help-markdown)::string

### DESCRIPTION ###
is a procedure to execute when the user needs something which calms you down.
When this procedure is called, this procedure will return a message which tries to
calm the user down. Any argument specified to this procedure will be silently
ignored.This procedure is deliberately defined as a joke and has by no means effect to the
current system state nor any other related elements. See (help about-main).



--------------------------------------------------------

NOTE ON
====================

#### SYNOPSIS ####
    (note on|non)

### DESCRIPTION ###




--------------------------------------------------------

NOTE OFF
====================

#### SYNOPSIS ####
    (note off|noff)

### DESCRIPTION ###




--------------------------------------------------------

KEY-PRESSURE
====================

#### SYNOPSIS ####
    (key-pressure|kp)

### DESCRIPTION ###




--------------------------------------------------------

CONTROL-CHANGE
====================

#### SYNOPSIS ####
    (control-change|cc)

### DESCRIPTION ###




--------------------------------------------------------

PROGRAM
====================

#### SYNOPSIS ####
    (program|pc)

### DESCRIPTION ###




--------------------------------------------------------

CHANNEL-PRESSURE
====================

#### SYNOPSIS ####
    (channel-pressure|cp)

### DESCRIPTION ###




--------------------------------------------------------

PITCH-BEND
====================

#### SYNOPSIS ####
    (pitch-bend|pb)

### DESCRIPTION ###




--------------------------------------------------------

ALL-SOUND-OFF
====================

#### SYNOPSIS ####
    (all-sound-off|aso)

### DESCRIPTION ###




--------------------------------------------------------

RESET-ALL-CONTROLLERS
====================

#### SYNOPSIS ####
    (reset-all-controllers|rac)

### DESCRIPTION ###




--------------------------------------------------------

LOCAL-CONTROLS
====================

#### SYNOPSIS ####
    (local-controls|lc)

### DESCRIPTION ###




--------------------------------------------------------

ALL-NOTE-OFF
====================

#### SYNOPSIS ####
    (all-note-off|anf)

### DESCRIPTION ###




--------------------------------------------------------

OMNI-MODE-OFF
====================

#### SYNOPSIS ####
    (omni-mode-off|omff)

### DESCRIPTION ###




--------------------------------------------------------

OMNI-MODE-ON
====================

#### SYNOPSIS ####
    (omni-mode-on|omon)

### DESCRIPTION ###




--------------------------------------------------------

MONO-MODE-ON
====================

#### SYNOPSIS ####
    (mono-mode-on|mono)

### DESCRIPTION ###




--------------------------------------------------------

POLY-MODE-ON
====================

#### SYNOPSIS ####
    (poly-mode-on|poly)

### DESCRIPTION ###




--------------------------------------------------------

SYS-SONG-POSITION-POINTER
====================

#### SYNOPSIS ####
    (sys-song-position-pointer|spp)

### DESCRIPTION ###




--------------------------------------------------------

SYS-SONG-SELECT
====================

#### SYNOPSIS ####
    (sys-song-select|ss)

### DESCRIPTION ###




--------------------------------------------------------

SYS-END-OF-EXCLUSIVE
====================

#### SYNOPSIS ####
    (sys-end-of-exclusive|eoe)

### DESCRIPTION ###




--------------------------------------------------------

SYS-CLOCK
====================

#### SYNOPSIS ####
    (sys-clock|clock)

### DESCRIPTION ###




--------------------------------------------------------

SYS-START
====================

#### SYNOPSIS ####
    (sys-start|start)

### DESCRIPTION ###




--------------------------------------------------------

SYS-CONTINUE
====================

#### SYNOPSIS ####
    (sys-continue|cont)

### DESCRIPTION ###




--------------------------------------------------------

SYS-STOP
====================

#### SYNOPSIS ####
    (sys-stop|stop)

### DESCRIPTION ###




--------------------------------------------------------

SYS-RESET
====================

#### SYNOPSIS ####
    (sys-reset|reset)

### DESCRIPTION ###




--------------------------------------------------------

BANK-SELECT
====================

#### SYNOPSIS ####
    (bank-select|bs)

### DESCRIPTION ###
Bank Select Allows user to switch bank for patch selection. Program change used
with Bank Select. MIDI can access 16,384 patches per MIDI channel.



--------------------------------------------------------

MODULATION
====================

#### SYNOPSIS ####
    (modulation|mod)

### DESCRIPTION ###
Modulation Generally this CC controls a vibrato effect (pitch, loudness,
brighness). What is modulated is based on the patch.



--------------------------------------------------------

BREATH-CONTROLLER
====================

#### SYNOPSIS ####
    (breath-controller|bc)

### DESCRIPTION ###
Breath Controller Often times associated with aftertouch messages. It was
originally intended for use with a breath MIDI controller in which blowing harder produced
higher MIDI control values. It can be used for modulation as well.



--------------------------------------------------------

FOOT-CONTROLLER
====================

#### SYNOPSIS ####
    (foot-controller|fc)

### DESCRIPTION ###
Foot Controller Often used with aftertouch messages. It can send a continuous
stream of values based on how the pedal is used.



--------------------------------------------------------

PORTAMENTO-TIME
====================

#### SYNOPSIS ####
    (portamento-time|pt)

### DESCRIPTION ###
Portamento Time Controls portamento rate to slide between 2 notes played subsequently.



--------------------------------------------------------

DATA-ENTRY-MSB
====================

#### SYNOPSIS ####
    (data-entry-msb|de-msb)

### DESCRIPTION ###
Data InitializerEntry Most Significant Bit(MSB) Controls Value for NRPN or RPN parameters.



--------------------------------------------------------

VOLUME
====================

#### SYNOPSIS ####
    (volume|v)

### DESCRIPTION ###
Volume Control the volume of the channel



--------------------------------------------------------

BALANCE
====================

#### SYNOPSIS ####
    (balance|b)

### DESCRIPTION ###
Balance Controls the left and right balance, generally for stereo patches.0 =
hard left, 64 = center, 127 = hard right



--------------------------------------------------------

PAN
====================

#### SYNOPSIS ####
    (pan|p)

### DESCRIPTION ###
Pan Controls the left and right balance, generally for mono patches.0 = hard
left, 64 = center, 127 = hard right



--------------------------------------------------------

EXPRESSION
====================

#### SYNOPSIS ####
    (expression|e)

### DESCRIPTION ###
Expression Expression is a percentage of volume (CC7).



--------------------------------------------------------

EFFECT-CONTROLLER-1
====================

#### SYNOPSIS ####
    (effect-controller-1|ec1)

### DESCRIPTION ###
Effect Controller 1 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

EFFECT-CONTROLLER-2
====================

#### SYNOPSIS ####
    (effect-controller-2|ec2)

### DESCRIPTION ###
Effect Controller 2 Usually used to control a parameter of an effect within the synth/workstation.



--------------------------------------------------------

SUSTAIN-PEDAL
====================

#### SYNOPSIS ####
    (sustain-pedal|sp)

### DESCRIPTION ###
Damper Pedal / Sustain Pedal On/Off switch that controls sustain. (See also
Sostenuto CC 66)0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

PORTAMENTO-SWITCH
====================

#### SYNOPSIS ####
    (portamento-switch|ps)

### DESCRIPTION ###
Portamento On/Off Switch On/Off switch0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOSTENUTO-SWITCH
====================

#### SYNOPSIS ####
    (sostenuto-switch|sos-s)

### DESCRIPTION ###
Sostenuto On/Off Switch On/Off switch – Like the Sustain controller (CC 64),
However it only holds notes that were “On” when the pedal was pressed. People use it to
“hold” chords” and play melodies over the held chord.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

SOFT-PEDAL-SWITCH
====================

#### SYNOPSIS ####
    (soft-pedal-switch|soft-pedal)

### DESCRIPTION ###
Soft Pedal On/Off Switch On/Off switch- Lowers the volume of notes played.0 to
63 = Off, 64 to 127 = On



--------------------------------------------------------

LEGATO-SWITCH
====================

#### SYNOPSIS ####
    (legato-switch|ls)

### DESCRIPTION ###
Legato FootSwitch On/Off switch- Turns Legato effect between 2 subsequent notes
On or Off.0 to 63 = Off, 64 to 127 = On



--------------------------------------------------------

HOLD-2
====================

#### SYNOPSIS ####
    (hold-2|h2)

### DESCRIPTION ###
Hold 2 Another way to “hold notes” (see MIDI CC 64 and MIDI CC 66). However
notes fade out according to their release parameter rather than when the pedal is released.



--------------------------------------------------------

SOUND-CONTROLLER-1
====================

#### SYNOPSIS ####
    (sound-controller-1|sc1)

### DESCRIPTION ###
Sound Controller 1 Usually controls the way a sound is produced. Default =
Sound Variation.



--------------------------------------------------------

SOUND-CONTROLLER-2
====================

#### SYNOPSIS ####
    (sound-controller-2|sc2)

### DESCRIPTION ###
Sound Controller 2 Allows shaping the Voltage Controlled Filter (VCF). Default
= Resonance -also(Timbre or Harmonics)



--------------------------------------------------------

SOUND-CONTROLLER-3
====================

#### SYNOPSIS ####
    (sound-controller-3|sc3)

### DESCRIPTION ###
Sound Controller 3 Controls release time of the Voltage controlled Amplifier
(VCA). Default = Release Time.



--------------------------------------------------------

SOUND-CONTROLLER-4
====================

#### SYNOPSIS ####
    (sound-controller-4|sc4)

### DESCRIPTION ###
Sound Controller 4 Controls the “Attack’ of a sound. The attack is the amount
of time it takes forthe sound to reach maximum amplitude.



--------------------------------------------------------

SOUND-CONTROLLER-5
====================

#### SYNOPSIS ####
    (sound-controller-5|sc5)

### DESCRIPTION ###
Sound Controller 5 Controls VCFs cutoff frequency of the filter.



--------------------------------------------------------

SOUND-CONTROLLER-6
====================

#### SYNOPSIS ####
    (sound-controller-6|sc6)

### DESCRIPTION ###
Sound Controller 6 Generic – Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-7
====================

#### SYNOPSIS ####
    (sound-controller-7|sc7)

### DESCRIPTION ###
Sound Controller 7 Generic – Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-8
====================

#### SYNOPSIS ####
    (sound-controller-8|sc8)

### DESCRIPTION ###
Sound Controller 8 Generic – Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-9
====================

#### SYNOPSIS ####
    (sound-controller-9|sc9)

### DESCRIPTION ###
Sound Controller 9 Generic – Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

SOUND-CONTROLLER-10
====================

#### SYNOPSIS ####
    (sound-controller-10|sc10)

### DESCRIPTION ###
Sound Controller 10 Generic – Some manufacturers may use to further shave their sounds.



--------------------------------------------------------

GENERAL-PURPOSE-CC-01
====================

#### SYNOPSIS ####
    (general-purpose-cc-01|gp01)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-02
====================

#### SYNOPSIS ####
    (general-purpose-cc-02|gp02)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-03
====================

#### SYNOPSIS ####
    (general-purpose-cc-03|gp03)

### DESCRIPTION ###
General PurposeMIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

GENERAL-PURPOSE-CC-04
====================

#### SYNOPSIS ####
    (general-purpose-cc-04|gp04)

### DESCRIPTION ###
General Purpose MIDI CC Controller GenericOn/Off switch0 to 63 = Off, 64 to 127
= On



--------------------------------------------------------

PORTAMENTO
====================

#### SYNOPSIS ####
    (portamento|po)

### DESCRIPTION ###
Portamento CC Control Controls the amount of Portamento.



--------------------------------------------------------

EFFECT-1
====================

#### SYNOPSIS ####
    (effect-1|e1)

### DESCRIPTION ###
Effect 1 Depth Usually controls reverb send amount



--------------------------------------------------------

EFFECT-2
====================

#### SYNOPSIS ####
    (effect-2|e2)

### DESCRIPTION ###
Effect 2 Depth Usually controls tremolo amount



--------------------------------------------------------

EFFECT-3
====================

#### SYNOPSIS ####
    (effect-3|e3)

### DESCRIPTION ###
Effect 3 Depth Usually controls chorus amount



--------------------------------------------------------

EFFECT-4
====================

#### SYNOPSIS ####
    (effect-4|e4)

### DESCRIPTION ###
Effect 4 Depth Usually controls detune amount



--------------------------------------------------------

EFFECT-5
====================

#### SYNOPSIS ####
    (effect-5|e5)

### DESCRIPTION ###
Effect 5 Depth Usually controls phaser amount



--------------------------------------------------------

DATA-INCREMENT
====================

#### SYNOPSIS ####
    (data-increment|inc)

### DESCRIPTION ###
(+1) Data Increment Usually used to increment data for RPN and NRPN messages.



--------------------------------------------------------

DATA-DECREMENT
====================

#### SYNOPSIS ####
    (data-decrement|dec)

### DESCRIPTION ###
(-1) Data Decrement Usually used to decrement data for RPN and NRPN messages.



--------------------------------------------------------

NRPN-LSB
====================

#### SYNOPSIS ####
    (nrpn-lsb|nrpn-l)

### DESCRIPTION ###
Non-Registered Parameter Number LSB (NRPN) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

NRPN-MSB
====================

#### SYNOPSIS ####
    (nrpn-msb|nrpn-m)

### DESCRIPTION ###
Non-Registered Parameter Number MSB (NRPN) For controllers 6, 38, 96, and 97,
it selects the NRPN parameter.



--------------------------------------------------------

RPN-LSB
====================

#### SYNOPSIS ####
    (rpn-lsb|rpn-l)

### DESCRIPTION ###
Registered Parameter Number LSB (RPN) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

RPN-MSB
====================

#### SYNOPSIS ####
    (rpn-msb|rpn-m)

### DESCRIPTION ###
Registered Parameter Number MSB (RPN) For controllers 6, 38, 96, and 97, it
selects the RPN parameter.



--------------------------------------------------------

NO-OPERATION
====================

#### SYNOPSIS ####
    (no-operation|nop)

### DESCRIPTION ###




--------------------------------------------------------

NOTE-ON-OFF
====================

#### SYNOPSIS ####
    (note-on-off|note [XXX|port::string|number=0] [XXX|chan::number=0] [XXX|pos::number=0.0] [XXX|note::number=60(C4)] [XXX|velo::number=0.5] [XXX|len::number=0.0025d])

### DESCRIPTION ###
This denotes a musical note. This notation object causes the sequencer to
automatically send both Note On MIDI event and Note Off MIDI event.



--------------------------------------------------------

LENGTH
====================

#### SYNOPSIS ####
    (length|len [XXX|val::number=0.0])

### DESCRIPTION ###
length specifies the measure length. This notation object specifies the total
measure length of the current notation object set.



--------------------------------------------------------

EXECUTE
====================

#### SYNOPSIS ####
    (execute|exec [XXX|val::number=0.0])

### DESCRIPTION ###
execute invokes the specific procedure. This notation object specifies the
total measure length of the current notation object set.



--------------------------------------------------------

ADD-TRACK
====================

#### SYNOPSIS ####
    (add-track|add)

### DESCRIPTION ###




--------------------------------------------------------

KILL-TRACK
====================

#### SYNOPSIS ####
    (kill-track|kil)

### DESCRIPTION ###




--------------------------------------------------------

DELETE-TRACK
====================

#### SYNOPSIS ####
    (delete-track|del)

### DESCRIPTION ###




--------------------------------------------------------

END-TRACK
====================

#### SYNOPSIS ####
    (end-track|end)

### DESCRIPTION ###




--------------------------------------------------------


