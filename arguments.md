Command-line Parameter Reference
=================================

Lamu's command-line parameter has two modes; one is `default mode` and the 
other is `advanced mode`.

# Command-line Parameter in Default Mode #

```bash
lamu [Scheme filename]
```

This demonstrates how to start the new Lamu application instance in the 
default-mode. The filename argument is optional; if the filename argument is 
given, then the Lamu's main-editor opens
the specified file.


# Command-line Parameter in Advanced Mode #

## Overview ##
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

```bash
> lamu do (....)
```

## `exec` Command ##

In advanced mode, more complicated commands can be specified after the `do` 
keyword.

```bash
> lamu do exec scheme + pulsar + gui +
```

The above is an example which demonstrates how to use `exec` command of the 
advanced mode.  `exec` instantiates the specified components of Lamu. This 
example instantiates a Scheme engine, a Pulsar instance and a GUI frame. They  
are the most basic set of components in Lamu. Note that the every component 
name is separated by keyword `+`.  These `+` tokens separates their regions. 

```bash
> lamu do exec scheme + pulsar + gui /path/to/file.scm +
```

The above is an example which executes Lamu with the basic set of the 
components, and then the main editor opens the specified file 
`/path/to/file.scm`.  Note that the every region which is separated by `+` 
tokens contains multiple elements. And the first element of a region
denotes the name of the component to instantiate.

### Multi-Statements ###

```bash
> lamu do begin exec scheme + pulsar + gui + end begin exec scheme + pulsar + gui + end
```

The above is an example of multiple commands. It is able to specify multiple 
sessions of advanced commands. This creates two distinctive Scheme engines and 
then connect a Scheme editor to each in a same Java Virtual Machine. That is, 
both editor has its own variable scope.

In editor one, execute the following code.

```scheme
(define v 'hello)
(display v)
(newline)
; => hello
```
And then in editor two, execute the following code.

```scheme
(define v 'world)
(display v)
(newline)
; => world
```

Then back to the editor one, execute the following code.

```scheme
(display v)
(newline)
; => hello
```

## `fork` Command ##

`fork` command invokes new Java Virtual Machine and load Lamu with
the specified arguments. Its primary usage is to configure Lamu
in client-server model. See the following example:

```bash
> lamu do begin exec scheme --server-port=8193 + kawapad + end begin fork do exec scheme + httpd --port=8193 + end
```
The above is an example to invoke and configure Lamu as client-server model. 

Lamu can run in client-server model. A Java editor is sometimes a burden for 
the garbage collection and obstructs JACKAudio's real-time processing. This 
causes unpredictable skips on the generated sound.  Therefore, Kawapad is 
designed that to be able to be separately executed in another Java Virtual 
Machine.

### Nested Multiple-Statements ###

Multiple-Statements can be nested; it can be used in `fork` command to give 
multiple-statements as its arguments.

```bash
> lamu do begin exec scheme --server-port=8193 + kawapad + end begin fork do begin exec scheme + httpd --port=8193 + end begin exec scheme + kawapad end end
```


```bash
> lamu do ([exec|fork|...]) ([argument]...)
```

<!--
# Command Specifier #
The first element in the command-line arguments is a command specifier. This 
can be one of `exec` or `fork`. Or in fact, this can be other string literals 
too, because it can be dynamically extended by the Lamu configuration file.
The mechanism will be described later. When the string as the command specifier 
is none of `exec` , `fork` nor other extended command names, the command 
specifier `default` is applied.
-->


