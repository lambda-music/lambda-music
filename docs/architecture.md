The Architecture of Lambda Music Sequencer
==========================================

Lambda Music Sequencer(hereinafter abbreviated as Lamu) is merely an Lisp 
editor built on Scheme interpreter works with JACKAudio client. This can be 
used as a live-coding environment, or a JACK connection controller which can be 
executed from your shell. I believe it is a very simple yet effective way to 
implement DAW in Linux. 

I have not tried other live-coding environment such as SuperCollider, Sonic Pi 
and others; the main difference between them and Lamu is that these 
environments have sound modules but Lamu does not. Lamu is simply a JACKAudio 
client. I believe the combination of Scheme and JACKAudio can be a very 
effective and "Linux"-way to implement a live-coding environment.

# The Architecture of Pulsar Sequencer #
Lamu consists several layers on its architecture.

![Architecture][LNK_ARCHITECTURE]

Lamu depends on following projects :
- [Kawa Scheme][LNK_KAWA]
  A Scheme implementation which is written in Java.
- [jna][LNK_JNA]
  Java Native Access.
- [jnajack][LNK_JNAJACK]
  Java bindings to JACK Audio Connection Kit.

Lamu includes following projects:

- [Lamu/Kawapad](../workspace/kawapad/)
  KawaPad is an Scheme editor which can write and run Scheme code on-the-fly.  
  The editor can be extended by Scheme itself.  Kawapad has a number of useful 
  features for editing Scheme programs. Kawapad is written in Java and 
  cooperatively works with [Kawa Scheme][LNK_KAWA].

- Lamu/KawaHTTP
  A HTTP interface for Kawa. This allows remote processes to execute any Scheme 
  code remotely.

- [Lamu/Metro](../workspace/metro/)
  Metro is a multi-track MIDI sequencer framework which is written in Java.
  Metro provides a simple framework to build music sequencing systems. Metro
  encapsulates the access to JACKAudio and offers mechanisms to send notation 
  data to JACKAudio.

- [Lamu/Pulsar](../workspace/pulsar/)
  Pulsar is the main component of this application; Pulsar works as a bridge 
  between [Metro](../workspace/metro/) and [Kawa Scheme][LNK_KAWA]. Pulsar also
  provides several sets of API for Scheme to control JACKAudio and others.

- [Lamu](../workspace/lamu/)
  The project for Lamu application class which manages command-line parsing,
  starting up and others.


# Lamu Client-Server Mode #
Lamu is also be able to run in client-server model. A Java text editor is 
sometimes a burden for the garbage collection and obstructs JACKAudio's 
real-time processing. This causes unpredictable skips on the generated sound.  
Therefore, Kawapad is designed that to be able to be separately executed in 
another JVM from the Java virtual-machine which processing the events of 
JACKAudio.  

This can be invoked by [Lamu Command-line parameter](./arguments.md).
For further information, please read the [Lamu Command-line 
parameter](./arguments.md).




[LNK_ARCHITECTURE]:https://lambda-music.github.io/lamu/imgs/lambda-music-architecture-300.png
[LNK_KAWA]: https://www.gnu.org/software/kawa/
[LNK_JNA]:https://github.com/java-native-access/jna
[LNK_JNAJACK]:https://github.com/jaudiolibs/jnajack
