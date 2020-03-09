Command-line Parameter Reference
=================================

Lamu is formed by several components. Lamu's command-line parameter can specify 
which components to be instantiated at the start-up. The overview about Lamu's 
architecture can be read at [The architecture of Lamu](./architecture.md).

For example, Lamu has a HTTP server component which enables remote clients to 
execute Scheme command on the server where Lamu is running. And Lamu also has a 
HTTP client component which enables accessing to the HTTP server. Lamu's 
command-line parameter cant specify how Lamu should run at the boot-time.

```
> lamu ([exec|fork|...]) ([argument]...)
```

# Command Specifier #
The first element in the command-line arguments is a command specifier. This 
can be one of `exec` or `fork`. Or in fact, this can be other string literals 
too, because it can be dynamically extended by the Lamu configuration file.
The mechanism will be described later. When the string as the command specifier 
is none of `exec` , `fork` nor other extended command names, the command 
specifier `default` is applied.







```
lamu  [exec|fork|""] 
lamu begin exec scheme + kawapad-gui + end begin fork exec scheme + pulsar + scheme-server + end

```

Lamu can run in client-server model. A Java editor is sometimes a burden for 
the garbage collection and obstructs JACKAudio's real-time processing. This 
causes unpredictable skips on the generated sound.  Therefore, Kawapad is 
designed that to be able to be separately executed in another JVM 
