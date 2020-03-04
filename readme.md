
Lambda Programmable Music Sequencer 
===================================

# Introduction #
Please, read [Overview of Lambda Programmable Music Sequencer][L_LAMBDA_MUSIC].

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

![Architecture][ARCHITECTURE]

Lamu depends on following projects :
- [Kawa Scheme][L_KAWA]
  A Scheme implementation which is written in Java.
- [jna][L_JNA]
  Java Native Access.
- [jnajack][L_JNAJACK]
  Java bindings to JACK Audio Connection Kit.

Lamu includes following projects :
- [Lamu/Kawapad](./workspace/kawapad/)
  A Lisp editor which is written in Java. This works as an interface for Kawa.
- Lamu/Kawa HTTP
  A HTTP interface for Kawa.
- [Lamu/Metro](./workspace/metro/)
  A multi-track MIDI sequencer framework which is written in Java.
- [Lamu/Pulsar](./workspace/pulsar/)
  A bridge between [Metro](./workspace/metro/) and [Kawa Scheme][L_KAWA]

# Documentation #

## User Documentation ##
- [Getting Started](./getting-started.md)
- [Lamu Command-line Parameter Specification](./workspace/build/docs/args-api/)
- [Pulsar API Reference](./workspace/build/docs/procs-api/)
- [Pulsar Notation Reference](./workspace/build/docs/notes-api/)
- [KawaPad API Reference](./workspace/kawapad/docs/api/)
- [KawaPad Keystroke Reference](./workspace/kawapad/docs/keystrokes/)

[# kawapad-api]: ./workspace/kawapad/docs.src/manual-kawapad-api.md
[# kawapad-keystroke]: ./workspace/kawapad/docs.src/manual-kawapad-keystroke.md

## Developer Documentation ##
- [Javadoc of Lambda Programmable Music Sequencer](./workspace/build/javadoc/)
- [Pulsar](./workspace/pulsar/readme.md)
- [Metro](./workspace/metro/readme.md)
- [KawaPad](./workspace/kawapad/readme.md)

# System Requirements #
Any operating systems that can run the following systems:
- Java 8
- JNA
- JACK Audio Connection Kit

Lamu has been developed and tested in Ubuntu 16.04. It runs under Windows 10
successfully. Running under OS X is still not tested; it should work as well 
operation systems.

Pulser uses following libraries :

- JNA-4.5.0
- JNAJACK-1.3.0
- KAWA-3.0
- JTattoo-1.6.11

These are statically linked and packed to `lamu.jar`.

# How to Install #

## Prerequisites ##
Lamu requires [Java8](https://www.java.com/en/download/) and [JACKAudio
Connection Kit](http://jackaudio.org/). These libraries should have properly 
installed on your environment before executing `lamu.jar`.

After the libraries are installed, download the `lamu.jar` from
[lamu.jar on the MASTER branch](https://github.com/lambda-music/lamu/blob/master/workspace/lamu/lamu.jar)

Lamu is packed into an executable JAR file and no installation is required.  
Just place the file any directory; preferably, it has better to be placed on a 
directory which is on the `$PATH` list.

# How to Run #

In a shell command prompt, execute

```bash
> java -jar /the-path-to-the-file/lamu.jar
```

In most platforms, you can also execute the application by double-clicking on
the file in your file-browser. 

## Command-line Parameter ##

Specifying a path to an arbitrary file as command-line parameter causes the 
Lamu editor to open the specified file.

```bash
> java jar lamu.jar any-scheme-program.scm
```

## Advanced Command-line Parameter ##
Lamu is composed by several components. And Lamu's command-line parameter can 
specify the components to be instantiated on at the start-up.

For further information, please read 
[Lamu Command-line Parameter Specification](./workspace/lamu/docs/args-api/)


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


[L_KAWA]: https://www.gnu.org/software/kawa/
[L_LAMBDA_MUSIC]:https://lambda-music.github.io/
[ARCHITECTURE]:https://lambda-music.github.io/lamu/imgs/lambda-music-architecture-300.png
[L_JNA]:https://github.com/java-native-access/jna
[L_JNAJACK]:https://github.com/jaudiolibs/jnajack
[editor-movie]:./imgs/corresponding-parenthesis-movement.gif

[vim-modeline]: # ( vim: set spell expandtab fo+=awlt : )
