Command-line Parameter Reference
=================================

Lamu's command-line parameter has two modes; one is `default mode` and the 
other is `advanced mode`.

# Command-line Parameter in Default Mode #

```
lamu [Scheme filename]
```

This demonstrates how to start the new Lamu application instance in the 
default-mode. The filename argument is optional; if the filename argument is 
given, then the Lamu's main-editor opens
the specified file.


# Command-line Parameter in Advanced Mode #

Lamu is formed by several components. Lamu's command-line parameter can specify 
which components to be instantiated at the start-up. Please read the overview 
of Lamu's architecture; it can be read at [The architecture of 
Lamu](./architecture.md).

For example, Lamu has a HTTP server component which enables remote clients to 
execute Scheme command on the server where Lamu is running. And Lamu also has a 
HTTP client component which enables accessing to the HTTP server. Lamu's 
command-line parameter cant specify how Lamu should run at the boot-time.

In order to enable the advanced-mode of command-line parameter, add a keyword 
`do` in front of other arguments. 

```
> lamu do (....)
```

In advanced mode, more complicated commands can be specified after the `do` 
keyword.

```
> lamu do exec scheme + pulsar + gui +
```

The above is an example which demonstrates how to use `exec` command of the 
advanced mode.  `exec` instantiates the specified components of Lamu, and this 
example instantiates a Scheme engine and Pulsar and its GUI which are the most 
basic set of components in Lamu. Note that the every component name is 
separated by keyword `+`.  These `+` tokens separates their regions. 

```
> lamu do exec scheme + pulsar + gui /path/to/file.scm +
```

The above is an example executes Lamu and opens the specified file 
`/path/to/file.scm`. Note that the every region which is separated by `+` 
tokens each contains multiple arguments. And the first element of a region
denotes the name of the component to instantiate.


```
> lamu do ([exec|fork|...]) ([argument]...)
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
