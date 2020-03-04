Lamu Architecture
=================

Lamu is merely an Lisp editor built on Scheme interpreter works with JACKAudio 
client. This can be used as a live-coding environment, or a JACK connection 
controller which can be executed from your shell. I believe it is a very simple 
yet effective way to implement DAW in Linux. 

I have not tried other live-coding environment such as SuperCollider, Sonic Pi 
and others; the main difference between them and Lamu is that these 
environments have sound modules but Lamu does not. Lamu is simply a JACKAudio 
client. I believe the combination of Scheme and JACKAudio can be a very 
effective and "Linux"-way to implement a live-coding environment.

### Architecture of Pulsar Sequencer ###
Lamu consists several layers on its architecture.

![Architecture][LNK_ARCHITECTURE]

Lamu depends on following projects :
- [Kawa Scheme][LNK_KAWA]
  A Scheme implementation which is written in Java.
- [jna][LNK_JNA]
  Java Native Access.
- [jnajack][LNK_JNAJACK]
  Java bindings to JACK Audio Connection Kit.

Lamu includes following projects :
- [Lamu/Kawapad](./workspace/kawapad/)
  A Lisp editor which is written in Java. This works as an interface for Kawa.
- Lamu/Kawa HTTP
  A HTTP interface for Kawa.
- [Lamu/Metro](./workspace/metro/)
  A multi-track MIDI sequencer framework which is written in Java.
- [Lamu/Pulsar](./workspace/pulsar/)
  A bridge between [Metro](./workspace/metro/) and [Kawa Scheme][LNK_KAWA]
- [Lamu](./workspace/lamu/)
  The project for Lamu application class which manages command-line parsing,
  starting up and others.


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

[LNK_ARCHITECTURE]:https://lambda-music.github.io/lamu/imgs/lambda-music-architecture-300.png
[LNK_KAWA]: https://www.gnu.org/software/kawa/
[LNK_JNA]:https://github.com/java-native-access/jna
[LNK_JNAJACK]:https://github.com/jaudiolibs/jnajack
