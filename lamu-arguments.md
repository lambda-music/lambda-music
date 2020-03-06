Command-line Parameter Reference
=================================

Lamu consists several components. Lamu's command-line parameter can specify 
which components to be instantiated at the start-up.

For further information about the architecture of Lamu, please see [The 
architecture of Lamu](./lamu-architecture.md).

Lamu is also be able to run in client-server model. A Java editor is sometimes 
a burden for the garbage collection and obstructs JACKAudio's real-time 
processing. This causes unpredictable skips on the generated sound.  Therefore, 
Kawapad is designed that to be able to be separately executed in another JVM 
from the Java virtual-machine which processing the events of JACKAudio.






```
[mode]
```

