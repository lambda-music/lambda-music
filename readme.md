
Pulsar List Scheme Music Sequencer
===================================

Pulsar lets you write a piece of music by Lisp Scheme!

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
  Pulsar is the main component of this application; Pulsar bridges three
  components Metro, KawaPad and Kawa  in order to implement the accessibility
  to JackAudio from Lisp Scheme.



<!-- vim: set spell: -->
