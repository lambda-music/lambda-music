
Pulsar List Scheme Music Sequencer
===================================


### === Pulsar lets you write a piece of music by Lisp Scheme! ===###

Pulsar is written in Java and Lisp Scheme which is powered by Kawa a Java based
Scheme implementation. You can manage audio data by JackAudio a multiplatform
audio connection system via Java Native Access.

Pulsar runs on most major platforms which can run Java such as Windows, Mac-OSX
and Linux distributions. 

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

### How to Install ###

Pulsar requires Java8; please make sure Java8 is installed on your environment.
Download [pulsar.jar on the MASTER branch](https://github.com/lisp-scheme-music/pulsar/blob/master/workspace/pulsar/pulsar.jar)
and save the file anywhere convenient for you.  Currently Pulsar does not have
installer. Pulsar is a simple JAR (Java Archive) file and it is not necessary
to install to a specific directory. 

### How to Run ###

After making sure that your pulsar.jar is located in a directory which is
specified in the //PATH// environment, 

```bash
> java -jar pulsar.jar
```
is suffice to make it run. In most platforms, you can also execute the application
by double-clicking on the file in your file-browser.


### Command Line Arguments ###

Pulsar has two interfaces : HTTP interface and window interface.

```bash
> java jar pulsar.jar --no-gui 
```
This disables the window interface.


```bash
> java jar pulsar.jar --no-http
```

This disables the HTTP interface.


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

Such kind of trick should easily be implemented in Emacs or other editors.


**Disclaimer**

Running Pulsar in a public computer which has a network interface with any
global IP assigned or a running production server etc. causes great security
risks. Do not run Pulsar in such situations.




<!-- vim: set spell: -->
