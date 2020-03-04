Command-line Parameter Reference
=================================

Lamu consists several components. It is able to be specified which components 
should be instantiated at the runtime. Lamu's command-line parameter specifies 
the component specs.

Lamu is also be able to run in client-server model. A Java editor can sometimes 
be a big burden for the garbage collection; A editor can be separately executed 
in another JVM from the JVM which runs Scheme engine. The command-line supports
such case.

# Syntax #

```
[type]
```

