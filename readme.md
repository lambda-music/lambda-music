
Lambda Programmable Music Sequencer 
===================================

# Introduction #
Please, read [Overview of Lambda Programmable Music Sequencer][lambda-music].

# Architecture #
Lamu is merely an Lisp editor built on Scheme interpreter works with JACKAudio 
client. This can be used as a live-coding environment, or a JACK connection 
controller which can be executed from your shell. I believe it is a very simple 
yet effective way to implement DAW in Linux. 

I have not tried other live-coding environment such as SuperCollider, Sonic Pi 
and others; the main difference between them and Lamu is that these 
environments have sound modules but Lamu does not. Lamu is simply a JACKAudio 
client. I believe the combination of Scheme and JACKAudio can be a very 
effective and "Linux"-way to implement a live-coding environment.

Lamu consists several layers on its architecture.

![Architecture][architecture]

Lamu depends on following projects :
- [Kawa Scheme][kawa]
  A Scheme implementation which is written in Java.
- [jna][jna]
  Java Native Access.
- [jnajack][jnajack]
  Java bindings to JACK Audio Connection Kit.

Lamu includes following projects :
- [Lamu/Kawapad][kawapad]
  A Lisp editor which is written in Java. This works as an interface for Kawa.
- Lamu/Kawa HTTP
  A HTTP interface for Kawa.
- [Lamu/Metro][metro]
  A multi-track MIDI sequencer framework which is written in Java.
- [Lamu/Pulsar][pulsar]
  A bridge between [Metro][metro] and [Kawa Scheme][kawa]

# Documentation #

## User Documentation ##
- [Getting Started](./getting-started.md)
- [Lamu Command-line Parameter Specification](./workspace/lamu/docs/args-api/) 
- [Pulsar API Reference](./workspace/lamu/docs/procs-api/)
- [Pulsar Notation Reference](./workspace/lamu/docs/notes-api/)
- [KawaPad API Reference](workspace/kawapad/docs/api/)
- [KawaPad Keystroke Reference](workspace/kawapad/docs/keystrokes/)

## Developer Documentation ##
- [Pulsar](./workspace/pulsar/)
- [Metro](./workspace/metro/)
- [JavaDoc of Metro](workspace/metro/doc/index.html)
- [KawaPad][kawapad]

[todo]: # ( aaa )

### System Requirements ###
Any operating systems that can run the following systems:
- Java 8
- JNA Java Native Access
- JACK Audio Connection Kit

Pulsar has been developed and tested in Ubuntu 16.04. A cursory experiment to
run Pulsar in Windows 10 with Windows JACKAudio was succeeded.  It is still unknown
if Pulsar can run in OS X and further experiments are needed.

Pulser uses following libraries :

- JNA-4.5.0
- JNAJACK-1.3.0
- KAWA-3.0
- JTattoo-1.6.11

These are statically linked to the main file `lamu.jar`; therefore, they are
not necessary to separately be installed.

### How to Install ###

Pulsar requires [Java8](https://www.java.com/en/download/) and [JACKAudio
Connection Kit](http://jackaudio.org/). Please make sure that these are
properly installed on your environment.

After these prerequisites are installed, just download the JAR file from
[lamu.jar on the MASTER branch](https://github.com/lambda-music/lamu/blob/master/workspace/lamu/lamu.jar).

Currently Pulsar have no installer. Though, Pulsar is a simple JAR (Java
Archive) file and no installation process is required. Just locate the file
anywhere convenient for you, preferably in any directory which is on your
//$PATH// list.


### How to Run ###

In your shell command prompt,
```bash
> java -jar /the-path-to-the-file/lamu.jar
```
is sufficient to make it run. 

In most platforms, you can also execute the application by double-clicking on
the file in your file-browser.

Pulsar has two interfaces : HTTP interface and window interface.
```bash
> java jar lamu.jar --no-gui 
```
This disables the window interface.


```bash
> java jar lamu.jar --no-http
```
This disables the HTTP interface.

```bash
> java jar lamu.jar [filename]
```
Otherwise every argument is taken as filename. Though, only the first argument
is applied; other arguments are silently ignored. Note that when //--no-gui//
is specified, no filename is applied since there is no editor to edit in that
case.

### How to Run (Advanced) ###

It is very interesting that Kawa can invoke every method on the fly; that is
you do not have to specify the startup class when you start the JVM. Add the
path of//lamu.jar// to your CLASSPATH then execute Kawa's REPL.

```bash
CLASSPATH="$CLASSPATH:/path-to-lamu-dir/lamu.jar" kawa
```
Then, execute the following command in Kawa's REPL.
```scheme
(lamu.LamuApplication:main (java.lang.String[]))
```
This also starts Lamu sequencer. This may give you some possibility to
control of the application more precisely.


### Execute Scheme Commands from Your Editors ###

If you are a VIM user, 
```VIM
:xmap <Return> :!curl -sSd "`cat`" http://localhost:8192/vim
```
this command effectively turns your VIM into a Scheme interactive editor. 
Select the specific text and hit your enter-key then the text will be executed 
in the Lamu application instance.

Lamu listens the port 8192 as a HTTP server, and executes any text which
comes via POST request from a client on localhost. It denies all requests from
hosts other than localhost.

Such kind of trick should easily be implemented in Emacs or other editors, too.


**Disclaimer**

Running Lamu in a public computer which has a network interface with any
global IP assigned or a running production server etc. causes great security
risks. Do not run Lamu in such situations.


### Basic  ###
_UNDER CONSTRUCTION_

### Command Reference ###
_UNDER CONSTRUCTION_

### Architecture of Pulsar Sequencer ###
_UNDER CONSTRUCTION_
Pulsar consists three parts of components :

- [KawaPad](./workspace/kawapad/readme.md )
  KawaPad is a simple editor which can be extended by writing Scheme code.
  It has a number of basic editor functions for writing Scheme programs. These
  functions are written by Scheme.

- [Metro](./workspace/metro/readme.md )
  Metro is a simple framework to build music sequencer systems. Metro
  encapsulates JACKAudio and offers mechanisms to send measure-beat based music
  data to JACKAudio.

- [Pulsar](./workspace/pulsar/readme.md )
  Pulsar is the main component of this application; Pulsar bridges between
  three components Metro, KawaPad and Kawa  in order to implement the
  accessibility to JACKAudio from Scheme.


### Compilation ###

Pulsar is developed by Eclipse and its repository contains entire
Eclipse's workspace directory.

In most case, opening the workspace directory by Eclipse should compile
the projects inside automatically. In case the project dependency could
not be restored properly, reconfigure it.

Currently there are four projects under Pulsar's workspace; each project
depends on following projects :

```memo
- lib
- kawapad
    +>lib
- metro 
    +-> lib
- pulsar 
    +-> lib
    +-> metro
    +-> kawapad
```

#### Compilation by Ant ####
`[workspace]/lamu/build-all.xml` is a ANT build file which was generated by
Eclipse. This file builds `Lamu.jar`.


[kawa]: https://www.gnu.org/software/kawa/
[lambda-music]: ../
[metro]:./workspace/metro/
[pulsar]:./workspace/pulsar/
[kawapad]:./workspace/kawapad/
[architecture]:https://lambda-music.github.io/lamu/imgs/lambda-music-architecture-300.png
[jna]:https://github.com/java-native-access/jna
[jnajack]:https://github.com/jaudiolibs/jnajack
[editor-movie]:./imgs/corresponding-parenthesis-movement.gif

[vim-modeline]: # ( vim: set spell expandtab fo+=awlt : )
