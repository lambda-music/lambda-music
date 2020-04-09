Lamu API Reference
======================


PULSAR
====================

#### SYNOPSIS ####
    (pulsar)

### DESCRIPTION ###
 



--------------------------------------------------------

OPEN?
====================

#### SYNOPSIS ####
    (open?)::boolean

### DESCRIPTION ###
||open?|| returns the current open state. This procedure returns \#t iff the
current sequencer state is open; otherwise returns \#f.



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

It is able to enumerate all ports by ||list-all-output|| and ||list-all-input||
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

It is able to enumerate all ports by ||list-all-output|| and ||list-all-input||
procedure. In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

LIST-ALL-OUTPUT
====================

#### SYNOPSIS ####
    (list-all-output|lao)::list<string>

### DESCRIPTION ###
retrieves IDs of all output connections in the current session of JACK Audio
Connection Kit. Each ID contains two parts which are separated by a separator character
":"; the former part is the server name part and the latter part is the port name
part.The passed arguments are silently discarded. In case the current sequencer system
has not established any connection to the JACK, it throws an exception.



--------------------------------------------------------

LIST-ALL-INPUT
====================

#### SYNOPSIS ####
    (list-all-input|lai)::list<string>

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
when \(rewind\) procedure is called in order to reset the sequencer's state.
Usually, the main procedure is a procedure to boot up the current song system.



--------------------------------------------------------

GET-MAIN
====================

#### SYNOPSIS ####
    (get-main)::procedure

### DESCRIPTION ###
retrieves the main procedure. See \(help set-main\) for further information.



--------------------------------------------------------

SET-PLAYING
====================

#### SYNOPSIS ####
    (set-playing playing::boolean)::void

### DESCRIPTION ###
sets the current playing state. When \#f is passed to this procedure, the
sequencer stops playing. When \#t is passed to this procedure ,the sequencer resumes
playing. In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

PLAYING?
====================

#### SYNOPSIS ####
    (playing?)::boolean

### DESCRIPTION ###
retrieves the current playing state. When the sequencer is playing, it returns
\#t; otherwise it returns \#f. See \(help set-playing\) for further information. In
case the current sequencer system has not established any connection to the JACK, it
throws an exception.



--------------------------------------------------------

PLAY
====================

#### SYNOPSIS ####
    (play)::void

### DESCRIPTION ###
causes the sequencer to start playing. See \(help set-play\) for further
information.In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



--------------------------------------------------------

STOP
====================

#### SYNOPSIS ####
    (stop)::void

### DESCRIPTION ###
causes the sequencer to stop playing. See \(help set-play\) for further
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
has the same effect with pressing the tap-tempo button on the main screen.
\(Tue, 07 Jan 2020 01:19:46 +0900\) Note : this description is obsolete. The
tap-tempo button is a button to change the current tempo. The button is supposed to be
pressed repeatedly to tell the system how fast the sequencer should play the current
music. The sequencer calculates the average of durations between the pressing the
button, and apply the value as the current tempo on the sequencer system. See \(help
set-tempo\).In case the current sequencer system has not established any connection to the
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

See \(help tap-tempo\) for further information.In case the current sequencer
system has not established any connection to the JACK, it throws an exception.



--------------------------------------------------------

RESET
====================

#### SYNOPSIS ####
    (reset)::void

### DESCRIPTION ###
resets the environment object of Scheme interpreter, and close the current JACK
connection. This procedure is supposed to be called interactively and is not supposed to be
called from other procedures; a procedure which called the \(reset\) procedure will be
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
usually causes the music sequencer to call the main-procedure. See \(help set-main\).
In case the current sequencer system has not established any connection to the
JACK, it throws an exception.



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
arguments. The tracks are stored in a linked list. See \(help about-track-spec\). In case
the current sequencer system has not established any connection to the JACK, it
throws an exception.



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
new-track procedure to the sequencer by \(put-track\) procedure. See \(help put-track\)
for further information. In case the current sequencer system has not established
any connection to the JACK, it throws an exception.



--------------------------------------------------------

NEW-RECORDING-TRACK
====================

#### SYNOPSIS ####
    (new-recording-track|rect [notations::procedure/(list notation)]...)::MetroTrack

### DESCRIPTION ###
new-recording-track creates a new track. In case the current sequencer system
has not established any connection to the JACK, it throws an exception.



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
manner. That is, \(car \(list-tracks\)\) always returns the last added track.



--------------------------------------------------------

CLEAR-TRACKS
====================

#### SYNOPSIS ####
    (clear-tracks|clet)::void

### DESCRIPTION ###
||clear-tracks|| removes all tracks on the current sequencer immediately.



--------------------------------------------------------

GET-MAIN-TRACK
====================

#### SYNOPSIS ####
    (get-main-track|getmt)::void

### DESCRIPTION ###
||get-main-track|| retrieves the reference to the current main track.



--------------------------------------------------------

GET-TRACK-POSITION
====================

#### SYNOPSIS ####
    (get-track-position|gettp)::void

### DESCRIPTION ###
||get-track-position|| gets the current position of the given track.



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

ADD-EVENT-LISTENER
====================

#### SYNOPSIS ####
    (add-event-listener target::object event-type::symbol callback::procedure)::void

### DESCRIPTION ###
||add-event-listener|| registers the specified procedure as an event handler.



--------------------------------------------------------

REMOVE-EVENT-LISTENER
====================

#### SYNOPSIS ####
    (remove-event-listener target::object callback::procedure)::void

### DESCRIPTION ###
||remove-event-listener|| unregisters the specified procedure as an event
handler.



--------------------------------------------------------

RANDOM
====================

#### SYNOPSIS ####
    (random|rnd [range::number=1])::number

### DESCRIPTION ###
||random|| generates a random number. This procedure adopts Mersenne Twister a
random number generating algorithm. If an argument \[range\] is specified, the return
value will be within 0<= x <\[range\]. If the argument is omitted, the range value
defaults to 1.



--------------------------------------------------------

LUCK
====================

#### SYNOPSIS ####
    (luck [probability::number=0.5])::boolean

### DESCRIPTION ###
||luck|| is a procedure that returns a random boolean value. The first
argument is the value of probability where the larger value causes the more probability
of returning \#t. When the specified value is equals or less than zero, the
returning value is always \#f. When the specified value is equals or larger than one the
returning value is always \#t. The only parameter can be omitted and in that case the
default value one is applied.



--------------------------------------------------------

