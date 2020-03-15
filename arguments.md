Command-line Parameter Reference
=================================

Lamu's command-line parameter has two modes: `default-mode` and `advanced-
mode`.

# Command-line Parameter in Default-Mode #

```bash
lamu [filename]
```

This demonstrates how to start the new Lamu application instance in the 
default-mode. The filename argument is optional. If the filename argument is 
given, then the Lamu's main-editor opens the specified file.


# Command-line Parameter in Advanced-Mode #

## Overview ##
Lamu is formed by several components. Lamu's command-line parameter can specify 
which components to be instantiated at the start-up. Please read the 
description at [The architecture of Lamu](./architecture.md).

For example, Lamu has a HTTP server component which enables remote clients to 
execute Scheme command on the server where Lamu is running. And Lamu also has a 
HTTP client component which enables accessing to the HTTP server. Lamu's 
command-line parameter cant specify how Lamu should run at the boot-time.

In order to enable the advanced-mode of command-line parameter, add a keyword 
`do` in front of other arguments. 

```
> lamu do ([exec|fork|...]) ([argument]...)
```

## `exec` Command ##

In advanced-mode, more complicated commands can be specified after the `do` 
keyword.

```
> lamu do exec scheme + pulsar + gui +
```

The above is an example which demonstrates how to use `exec` command of the 
advanced-mode.  `exec` instantiates the specified components of Lamu. This 
example instantiates a Scheme engine, a Pulsar instance and a GUI frame. They  
are the most basic set of components in Lamu. Note that the every component 
name is separated by keyword `+`.  These `+` tokens separates their regions. 

```
> lamu do exec scheme + pulsar + gui /path/to/file.scm +
```

The above is an example which executes Lamu with the basic set of the 
components, and then the main editor opens the specified file 
`/path/to/file.scm`.  Note that the every region which is separated by `+` 
tokens contains multiple elements. And the first element of a region
denotes the name of the component to instantiate.

## Multi-Statements ##

```
> lamu do begin exec scheme + pulsar + gui + end begin exec scheme + pulsar + gui + end
```

The above is an example to demonstrate how to use multiple-statements in the 
advanced-mode.  Note that there are two keywords : `begin` and `end`. Commands 
which surrounded by `begin` and `end` becomes a statement. 

This creates two distinctive Scheme engines and then connect a Scheme editor to 
each in a same JVM. That is, both editor has its own variable scope.

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

`fork` command invokes a new JVM instance and loads Lamu with
the specified arguments. Its primary usage is to configure Lamu
in client-server model. See the following example:

```
> lamu do begin exec scheme --server-port=8193 + kawapad + end begin fork do exec scheme + httpd --port=8193 + end
```
The above is an example to invoke and configure Lamu as client-server model. 

Lamu can run in client-server model. A Java editor is sometimes a burden for 
the garbage collection and obstructs JACKAudio's real-time processing. This 
causes unpredictable skips on the generated sound.  Therefore, Kawapad is 
designed that to be able to be separately executed in another JVM.

## Nested Multiple-Statements ##

Multiple-statements can be nested; a typical scenario of the usage of 
multiple-statement is using with `fork` command.

```
> lamu do begin exec scheme --server-port=8193 + kawapad + end begin fork do begin exec scheme + httpd --port=8193 + end begin exec scheme + kawapad end end
```

Note that `fork` command itself is surrounded by `begin` and `end` and its 
arguments also consist `begin` and `end`; this example works as expected.


## Defining User Commands ##

You can define new commands by creating macro-commands in the default-argument 
configuration file. The path of the configuration file is: 
`~/.lamu/default-arguments.conf` .

The following is an example of the configuration file.

```
default exec scheme + pulsar + repl + gui $*{--open=$} +
simple exec scheme + pulsar + simple-repl + gui $*{--open=$} +
cs exec begin scheme --server-port=8193 + gui $* end begin scheme + pulsar + httpd --port=8193 end
local exec scheme + pulsar + repl + gui $open{$} +
```

The first column of each line denotes the name of macro-command.

```
> lamu do local --open=/foo/bar/bum.scm
```

The above example is expanded as

```
> lamu do exec scheme + pulsar + repl + gui /foo/bar/bum.scm
```

`default` macro-command can override the behavior of the default-mode 
command-line parameter.

```
default exec scheme + pulsar + repl +
```

For example, setting as above makes `lamu` to execute without GUI as default.


## Available Lamu Component Reference ##

The following is the list of available components in `exec` command.

- kawapad
- pulsar
- repl
- gui
- httpd
- simple-repl
- output-help
- output-help-list

### `kawapad` ###
Instantiate Kawapad.
- `--server-url=[url]` specifies remote-server's URL.
- `--server-port=[port]` specifies remote-server's port. The address defaults 
  to `localhost`.

### `pulsar` ###
Instantiate Pulsar. No argument is available.

### `repl` ###
Instantiate a Scheme REPL processor. No argument is available.

### `gui` ###
Instantiate Lamu's default GUI. No argument is available.

### `httpd` ###
Instantiate a Scheme remote HTTP server.
- `--port=[port]` specifies the port to listen.
- `--path=[path]` specifies the server path to accept.

### `simple-repl` ###
Instantiate a simple REPL processor. 

### `output-help` ###
Output the specified command reference.
- `--category=[category-name]` specifies the category to output.
- `--output-file=[filename]` specifies the filename to output.

### `output-help-list` ###
Output the list of available command reference.
- `--output-file=[filename]` specifies the filename to output. If no output 
  file was specified, it outputs to `stdout`.




<!--
# Command Specifier #
The first element in the command-line arguments is a command specifier. This 
can be one of `exec` or `fork`. Or in fact, this can be other string literals 
too, because it can be dynamically extended by the Lamu configuration file.
The mechanism will be described later. When the string as the command specifier 
is none of `exec` , `fork` nor other extended command names, the command 
specifier `default` is applied.
-->


