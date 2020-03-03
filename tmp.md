
Lamu a Programmable Music Sequencer 
===================================

[Overview of Lambda Programmable Music Sequencer][lambda-music]

<iframe width="560" height="315" 
src="https://www.youtube.com/embed/4Uu6bKWs_Vc" frameborder="0" 
allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" 
allowfullscreen>
</iframe>

# Architecture #
Lamu is merely an Lisp editor built on Scheme interpreter works with JACKAudio 
client. This can be used as a live-coding environment, or a JACK connection 
controller which can be executed from your shell. I believe it is a very simple 
yet effective way to implement DAW in Linux. 

I have not tried other live-coding environment such as SuperCollider, Sonic Pi 
and others; the main difference between them and Lamu is that these 
environments have sound modules but Lamu does not. Lamu is simply a JACKAudio 
client. But I believe this can be a very Linux-way to implement a live-coding 
environment.

Lamu consists several layers on its architecture.

![Architecture][architecture]

- [Kawa Scheme][kawa]
  A Scheme implementation which is written in Java.
- Kawa HTTP
  A HTTP interface for Kawa.
- [Kawapad][kawapad]
  A Lisp editor which is written in Java. This works as an interface for Kawa.
- [Metro][metro]
  A multi-track MIDI sequencer framework which is written in Java.
- [Pulsar][pulsar]
  A bridge between [Metro][metro] and [Kawa Scheme][kawa]
- [jna][jna]
  Java Native Access.
- [jnajack][jnajack]
  Java bindings to JACK Audio Connection Kit.


This repository includes following three projects :
[Kawapad][kawapad] [Metro][metro] [Pulsar][pulsar]


I started to write this program to get a metronome to practice the guitar.
And then I realized that this program could be a live-coding environment; I 
started to write a Scheme editor to speed up the way write the Scheme code.  
That is [Kawapad][kawapad]

But later I realized that this program can be JACKAudio connection controller.  
Actually, I have been in a difficult time to get along with LADISH and I have 
never had it work right with my favorite synth applications. The connections 
are too fragile and lost too often. Therefore, I have written some mechanisms 
to restore/scrap a set of connections programmatically. That is 
[Pulsar][pulsar].  Not only is [Pulsar][pulsar] actually to restore/scrap 
connections; I'll talk about it later.

[Lamu](https://lambda-music.github.io/lamu/)

![corresponding-parenthesis-movement](imgs/corresponding-parenthesis-movement.gif)


### Documentation ###

- [Getting Started](getting-started.md)
- [Pulsar](workspace/pulsar/readme.md)
- [Pulsar API Reference](workspace/pulsar/readme-procs-api.md)
- [Pulsar Notation Reference](workspace/pulsar/readme-notes-api.md)
- [Metro](workspace/metro/readme.md)
- [JavaDoc of Metro](workspace/metro/doc/index.html)
- [KawaPad][kawapad]
- [KawaPad API Reference](workspace/kawapad/readme-api.md)


### Feature ###
- Enables you to write pieces of music as Scheme program.
- Built with Kawa a powerful Scheme implementation.
- Works with JACK Audio Connection Kit and is able to connect to any 
  synthesizer applications support JACK.
- Includes Kawapad; Kawapad is an editor to edit Scheme program 
	- Kawapad can prettify S-Expression.
	- Execute a block of code on-the-fly.
	- Kawapad can be extended by Kawa-Scheme.

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

These are statically linked to the main file `pulsar.jar`; therefore, they are
not necessary to separately be installed.

### How to Install ###

Pulsar requires [Java8](https://www.java.com/en/download/) and [JACKAudio
Connection Kit](http://jackaudio.org/). Please make sure that these are
properly installed on your environment.

After these prerequisites are installed, just download the JAR file from
[pulsar.jar on the MASTER
branch](https://github.com/lisp-scheme-music/pulsar/blob/master/workspace/pulsar/pulsar.jar).

Currently Pulsar have no installer. Though, Pulsar is a simple JAR (Java
Archive) file and no installation process is required. Just locate the file
anywhere convenient for you, preferably in any directory which is on your
//$PATH// list.


### How to Run ###

In your shell command prompt,
```bash
> java -jar /the-path-to-the-file/pulsar.jar
```
is sufficient to make it run. 

In most platforms, you can also execute the application by double-clicking on
the file in your file-browser.

Pulsar has two interfaces : HTTP interface and window interface.
```bash
> java jar pulsar.jar --no-gui 
```
This disables the window interface.


```bash
> java jar pulsar.jar --no-http
```
This disables the HTTP interface.

```bash
> java jar pulsar.jar [filename]
```
Otherwise every argument is taken as filename. Though, only the first argument
is applied; other arguments are silently ignored. Note that when //--no-gui//
is specified, no filename is applied since there is no editor to edit in that
case.

### How to Run (Advanced) ###

It is very interesting that Kawa can invoke every method on the fly; that is
you do not have to specify the startup class when you start the JVM. Add the
path of//pulsar.jar// to your CLASSPATH then execute Kawa's REPL.

```bash
CLASSPATH="$CLASSPATH:/path-to-pulsar-dir/pulsar.jar" kawa
```
Then, execute the following command in Kawa's REPL.
```scheme
(pulsar.Pulsar:main (java.lang.String[]))
```
This also starts Pulsar sequencer. This may give you some possibility to
control of the application more precisely.


### Execute Scheme Commands from Your Editors ###

If you are a VIM user, 
```VIM
:xmap <Return> :!curl -sSd "`cat`" http://localhost:8192/pulsar
```
this VIM command effectively turns your VIM into a Scheme interactive
editor. Select the specific text and hit your enter-key then the text will be
executed in the Pulsar application instance.

Pulsar listens the port 8192 as a HTTP server, and executes any text which
comes via POST request from a client on localhost. It denies all requests from
hosts other than localhost.

Such kind of trick should easily be implemented in Emacs or other editors, too.


**Disclaimer**

Running Pulsar in a public computer which has a network interface with any
global IP assigned or a running production server etc. causes great security
risks. Do not run Pulsar in such situations.


### Basic  ### 





### Command Reference ###


### Architecture of Pulsar Sequencer ###


Pulsar consists three parts of components :

- [KawaPad](./workspace/kawapad/readme.md )
  KawaPad is a simple editor which can be extended by writing Scheme code.
  It has a number of basic editor functions for writing Scheme programs. These
  functions are written by Scheme itself.

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
[workspace]/pulsar/build.xml is a ANT build file which was generated by
Eclipse. This build file can build Pulsar-Sequencer.



[jna]: https://github.com/java-native-access/jna
[kawa]: https://www.gnu.org/software/kawa/
[lambda-music]: https://lambda-music.github.io/
[metro]:https://lambda-music.github.io/lamu/workspace/metro/readme.md
[pulsar]:https://lambda-music.github.io/lamu/workspace/pulsar/readme.md
[kawapad]:https://lambda-music.github.io/lamu/workspace/kawapad/readme.md
[architecture]:https://lambda-music.github.io/lamu/imgs/lambda-music-architecture-300.png

[vim-modeline]: # ( vim: set spell expandtab fo+=awlt : )
