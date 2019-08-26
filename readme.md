
Pulsar a Lisp Scheme Music Sequencer
===================================


### Pulsar lets you write a piece of music by Lisp Scheme! ###

Pulsar is a music sequencer program which enables users to write music as Lisp
Scheme programs. In this system, musical notes and other informations are
written as Scheme's association lists. The musical notes can be dynamically
generated as Scheme's association lists on-the-fly. Users can also interact
with the dynamically generated music at runtime and affect the direction that
the music is going.

Pulsar is written in Java and Lisp Scheme which is powered by Kawa a Java based
Scheme implementation. You can process MIDI data via Jack Audio Connection Kit
a multiplatform audio connection system which is accessed via Java Native
Access.

Pulsar runs on most major platforms which can run Java such as Windows, Mac-OSX
and Linux distributions. 


### Index ###

[Pulsar](blob/master/workspace/pulsar/readme-api.md)
[Pulsar API Reference](blob/master/workspace/pulsar/readme-api.md)
[KawaPad](blob/master/workspace/kawapad/readme.md)
[KawaPad API Reference](blob/master/workspace/kawapad/readme-api.md)


### Feature ###
- Enables you to write pieces of music as Lisp Scheme program.
- Built with Kawa a powerful Lisp Scheme implementation.
- Works with JACK Audio Connection Kit and can connect to any synthesizer
  applications support JACK.
- Includes Kawapad; Kawapad is an editor to edit Scheme program 
	- Kawapad can prettify Lisp code.
	- Execute a block of code on-the-fly.
	- Kawapad can be extended by Kawa-Scheme.

### System Requirements ###
Any operating systems that can run the following systems :
- Java 8
- JNA Java Native Access
- Jack Audio Connection Kit

Pulsar has been developed and tested in Ubuntu 16.04. A cursory experiment to
run Pulsar in Windows 10 with Windows JACK was succeeded.  It is still unknown
if Pulsar can run in OS X and further experiments are needed.

Pulser uses following libraries :

- JNA-4.5.0
- JNAJACK-1.3.0
- KAWA-3.0

These are statically linked to the main file `pulsar.jar`.

### How to Install ###

Pulsar requires [Java8](https://www.java.com/en/download/) and [Jack Audio
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


### Execute Lisp Scheme Commands from Your Editors ###

If you are a VIM user, 
```VIM
:xmap <Return> :!curl -sSd "`cat`" http://localhost:8192/pulsar
```
this VIM command effectively turns your VIM into a Lisp Scheme interactive
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
  KawaPad is a simple editor which can be extended by writing Lisp Scheme code.
  It has a number of basic editor functions for writing Scheme programs. These
  functions are written by Lisp Scheme itself.

- [Metro](./workspace/metro/readme.md )
  Metro is a simple framework to build music sequencer systems. Metro
  encapsulates JackAudio and offers mechanisms to send measure-beat based music
  data to JackAudio.
  

- [Pulsar](./workspace/pulsar/readme.md )
  Pulsar is the main component of this application; Pulsar bridges between
  three components Metro, KawaPad and Kawa  in order to implement the
  accessibility to JackAudio from Lisp Scheme.


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





<!-- vim: set spell expandtab : -->
