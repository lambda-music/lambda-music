Lamu Command-line Parameter Reference
======================================

Lamu command-line parameter has two modes: `default-mode` and `advanced-
mode`. If the first argument is `advanced`, Lamu processes the arguments
in the `advanced-mode`; otherwise Lamu processes them as `default-mode`.

In this section, it presumes that the knowledge for the architecture of Lamu.
Please also read the description at [The architecture of 
Lamu](./architecture.md).

# Command-line Parameter in Default-Mode #

```bash
lamu [filename]
```

This demonstrates how to start a new Lamu application instance in the 
default-mode. The filename argument is optional. If any filename argument is 
given, then the Lamu's main-editor opens the specified file.

This default behavior can be customised. The way to customize the behavior is 
described later. 

# Command-line Parameter in Advanced-Mode #
Lamu's command-line parameter is a simple scripting language. Let's call it
_Lamu-Script_. The purpose of Lamu-Script is to specify how Lamu should run.

Lamu consists a number of components. And the command-line parameter can 
specify which components to be instantiated at boot-time. For example, Lamu has 
a HTTP server component which enables remote clients to execute Scheme command 
on the server where Lamu is running. And Lamu also has a HTTP client component 
which enables accessing to the HTTP server. The Lamu's advanced-mode 
command-line parameter can specify how Lamu should run.

In order to enable the advanced-mode, put a keyword `advanced` at the first 
argument.

```bash
> lamu advanced [command] ([argument]...)
```

Currently, there are three commands available:

- `create`
  Specify components to be instantiated.

- `fork`
  Execute external commands.

- `echo`
  Print the specified message.

- `load`
  Load the arguments from the specified file.

- `exec`
  Load files which are specified as the arguments; the files are concatenated 
  and treated as a Scheme module. And the first element in the module is 
  treated as a string value which specifies Lamu-Script to initialize the Lamu 
  application instance.


# `create` Command #

The `create` command specifies which component to be instantiated. For example:

```bash
> lamu advanced create scheme + pulsar + gui +
```

The above is an example which demonstrates how to use `create` command.  This 
example instantiates a Scheme engine, a Pulsar instance and a GUI frame.  They 
are the most basic set of components in Lamu. Note that the every component 
name is separated by keyword `+`.  These `+` tokens separates sections. Each 
section contains one or more arguments. The first argument denotes the name of 
the components to be instantiated and others are arguments for the component.

```bash
> lamu advanced create scheme + pulsar + gui /path/to/file.scm +
```

The above is an example which executes Lamu with the basic set of the 
components, and then the main editor opens the specified file 
`/path/to/file.scm`.  Note that the every region which is separated by `+` 
tokens contains multiple elements. And the first element of a region
denotes the name of the component to instantiate.

The available components are explained later.


# Multi-Line Arguments #
Lamu's advanced commands sometimes become very long. It is recommended to split
the commands when they come to a certain length by using Shell's escape 
sequence character as:

Before:
```bash
> lamu advanced create scheme + pulsar + gui /path/to/file.scm +
```

After:
```bash
> lamu advanced \
    create scheme + pulsar + \
    gui /path/to/file.scm
```

# Multi-Statements #
In advanced-mode, it is able to pass two or more commands at once. Let's call 
it `multi-statements`.

```bash
> lamu advanced \
    begin \
        create scheme + pulsar + gui + \
    end \
    begin \
        create scheme + pulsar + gui \
    end \
```

The above is an example to demonstrate how to use multiple-statements in the 
advanced-mode. Note that there are two keywords : `begin` and `end`. Commands 
which surrounded by `begin` and `end` becomes a statement. 

This creates two distinctive Scheme engines and then connect a Scheme editor to 
each in a same JVM. That is, both editor has its own variable scope. This usage 
is explained later.


# `fork` Command #
The `fork` command invokes a new JVM instance and loads Lamu with
the specified arguments. Its primary usage is to configure Lamu
in client-server mode. See the following example:

```bash
> lamu advanced \
    begin \
        fork lamu advanced create scheme + pulsar + http --port=8193 \
    end \
    begin \
        create scheme --http=http://localhost:8193 + kawapad song.scm \
    end \
```
The above is an example to invoke and configure Lamu in HTTP client-server mode. 

Lamu can run in client-server mode. A Java editor is sometimes a burden for 
the garbage collection and obstructs JACKAudio's real-time processing. This 
causes unpredictable skips on the generated sound.  Therefore, Kawapad is 
designed that to be able to be separately executed in another JVM.

# Nested Multiple-Statements #
Multiple-statements can be nested; a typical scenario of the usage of 
multiple-statement is using with `fork` command.

```bash
> lamu advanced \
    begin \
        create scheme --server-port=8193 + kawapad + \
    end \
    begin \
        fork lamu advanced \
        begin \
            create scheme + http --port=8193 + \
        end \
        begin \
            create scheme + kawapad \
        end \
    end \
```

Note that `fork` command itself is surrounded by `begin` and `end` and its 
arguments also consist `begin` and `end`; this example works as expected.

# `echo` Command #
Print the specified message.
```bash
> lamu advanced echo HELLO WORLD FOO BAR!
```

This command causes `HELLO WORLD FOO BAR!` to be printed.


# `load` Command #
The `load` command load the specified file and execute it as Lamu-Script 
program. 

For example, when there is a file `foo/bar/bum.lamu` as:

```bash
advanced
begin
    echo foo bar
end
begin
    create scheme + pulsar + repl + gui $*{$}
end
begin
    echo foo bar
end
```

And execute the file as:

```bash
> lamu advanced load foo/bar/bum.lamu
```

This causes Lamu to run with basic components with `foo bar` printed in
the standard stream.

Note that in the Lamu-Script file, multiple-line command-line are available 
without escape sequence characters.

# Dynamic Parameter in Lamu-Script #
Lamu-Script itself can accept arguments.

Create a file which name is `example.lamu` as:

```bash
advanced
begin
    echo $hello{FOO $ BAR}
end
```

Then, execute the script as:

```bash
advanced load 'example.lamu' --hello='HELLO WORLD' 2> /dev/null
```

This causes `FOO HELLO WORLD BAR` to be printed.

That is, the part `echo $hello{FOO $ BAR}`  is interpolated as `echo FOO HELLO 
WORLD BAR`.


## Specification ##
```
$VARIABLE-NAME{ ANY VALUE FORMAT SPEC $ FORMAT SPEC }
```
- Any argument starts with `$` is treated as a variable.
- The alphabet characters after the `$` are treated as a name of the variable.
- When the command-line processor encounters a variable in a Lamu-Script, the 
  processor replaces it to the content of the corresponding named-argument only 
  when the corresponding named-argument exists in the runtime-arguments; 
  otherwise the variable will be ignored and removed from the arguments.
- When there is a block which is surrounded by curly-braces after a variable,
  it is treated as format-specification value. The variable is replaced with 
  the value in the format-specification. If there is a `$` in the 
  format-specification, the `$` will be replaced with the corresponding 
  named-argument.



# Defining User Commands #
You can define new commands by creating macro-commands in the default-argument 
configuration file. The path of the configuration file is: 
`~/.lamu/default-arguments.conf` .

The following is an example of the configuration file.

```bash
default create scheme + pulsar + repl + gui $*{--open=$} +
simple create scheme + pulsar + simple-repl + gui $*{--open=$} +
cs create begin scheme --server-port=8193 + gui $* end begin scheme + pulsar + http --port=8193 end
local create scheme + pulsar + repl + gui $open{$} +
```

The first column of each line denotes the name of macro-command.

```bash
> lamu advanced local --open=/foo/bar/bum.scm
```

The above example is expanded as

```bash
> lamu advanced create scheme + pulsar + repl + gui /foo/bar/bum.scm
```

# Predefined Commands #

In fact, the default-mode command-line simply calls advanced-mode with a 
specific command name. The command name is `default`.

```
load advanced create scheme + pulsar + repl $*{--load=$}
open advanced create scheme + pulsar + repl + gui $*{$}
default open $*{$}
```

As you can see from the definition, `default` command effectively forwards its 
arguments to the `open` command.

If a user executes the following command:

```bash
> lamu path/to/file.scm
```

The expanded result should be:

```bash
advanced create scheme + pulsar + repl + gui path/to/file.scm
```

# Customization of Default-Mode of Command-line Parameter #

The default-mode command-line parameter accepts some flags:

- `--command`
- `--load`
- `--exec` / `-e`

## `--command` ##

The `--command`  flag can specify the command to call in the default-mode. As 
we see above, the default-mode forwards all of the arguments into `open`.  The 
`--command` flag can change this target command into the specified command.

```bash
> lamu --command=load path/to/file.lamu
```

The example above is identical as:

```bash
> lamu advanced load path/to/file.lamu
```


## `--load` ##

In fact, the `--load=path/to/file.lamu` flag is identical as `--command=load 
path/to/file.lamu`.  This command is designed due to the limitation of \*NIX's 
shebang line which can only accept one argument.

That is,

```bash
> lamu --load=path/to/file.lamu
```

The example above is identical as:

```bash
> lamu advanced load path/to/file.lamu
```

The main usage of the `--load` command is shebangs.

Consider create a file which name is `test.lamu` as following:

```bash
#!/usr/bin/lamu --load=path/to/file
advanced
begin
    create scheme + pulsar + repl $*{--load=$}
end
```

Then,

```bash
> chmod 755 ./test.lamu
> ./test.lamu
```

This executes Lamu with specified components.


## `--exec` / `-e` ##

In fact, the `--exec`/`-e` flag is identical as `--command=exec`.  See the 
description for `exec` command above.


# `exec` command

Load files which are specified as the arguments; the files are concatenated and 
treated as a Scheme module. And the first element in the module is treated as a 
string value which specifies Lamu-Script to initialize the Lamu application 
instance.

For example, create a file which name is `foo/bar/file.scm` as:

```scheme
#!/usr/bin/lamu -e
"advanced begin create scheme + pulsar + repl $*{--load=$}"

(display 'hello)
(newline)
(quit)
```

And execute :

```bash
> chmod 755 foo/bar/file.scm
> foo/bar/file.scm
```

The Lamu command-line parser load the `foo/bar/file.scm` then pick the
first element. In this case it is `advanced begin create scheme + pulsar + repl 
$*{--load=$}`. And then the command-line parser execute it as Lamu-Script.

When the command-line parser executes Lamu-Script, the arguments are 
transparently passed to the Lamu-Script. In case of the example above, the 
arguments are passed to `repl $*{--load=$}`.

That is, the files are processed in the `repl` component.




# Overriding the Default Command #

`default` macro-command can override the behavior of the default-mode 
command-line parameter.

```
default create scheme + pulsar + repl +
```

For example, setting as above makes `lamu` to execute without GUI as default.

# Lamu Component Reference #
The following is the list of available components in `create` command.

- kawapad
- pulsar
- repl
- gui
- http
- simple-repl
- logger-stream
- stdio-stream
- forked-stream
- reference
- reference-list

## `kawapad` ##
Instantiate Kawapad.
- `--server-url=[url]` specifies remote-server's URL.
- `--server-port=[port]` specifies remote-server's port. The address defaults 
  to `localhost`.

## `pulsar` ##
Instantiate Pulsar. No argument is available.

## `repl` ##
Instantiate a Scheme REPL processor. The `repl` component takes arguments as 
filenames; it concatenates all of the files and process it as commands for the 
REPL. For
further information about the syntax of REPL, see [Lamu REPL 
Specification](./repl.md).


## `gui` ##
Instantiate Lamu's default GUI. The `gui` component takes arguments as 
filenames; the instantiated GUI opens all of the specified files.

## `http` ##
Instantiate a Scheme remote HTTP server.
- `--port=[port]` specifies the port to listen.
- `--path=[path]` specifies the server path to accept.

## `simple-repl` ##
Instantiate a simple REPL processor. 

## `stdio-stream` ##
Create a standard stream object and push it on the stack.

## `forked-stream` ##
Retrieve the last forked process and create a stream object from it and push it 
on the stack.

## `logger-stream` ##
A logger-stream is a stream-wrapper which intercepts the data streams and save 
them to the specified files. The intercepted data are transparently passed to 
the following stream.

When `logger-stream` is specified, the system retrieves  a stream object from 
the stack and create a logger-stream, and then push it to the stack.

This accepts the following named-arguments:

- `--out=FILENAME`
  Specifies the filename to output the content of stdout.
- `--in=FILENAME`
  Specifies the filename to output the content of stdin.
- `--err=FILENAME`
  Specifies the filename to output the content of stderr.


## `reference` ##
Output the specified command reference.
- `--category=[category-name]` specifies the category to output.
- `--output-file=[filename]` specifies the filename to output.

## `reference-list` ##
Output the list of available command reference.
- `--output-file=[filename]` specifies the filename to output. If no output 
  file was specified, it outputs to `stdout`.


# Advanced Usage Examples #

## Instantiate Two or More Scheme Engines ##

```bash
> lamu advanced \
    begin \
        create scheme + pulsar + gui + \
    end \
    begin \
        create scheme + pulsar + gui \
    end \
```

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


<!--
# Command Specifier #
The first element in the command-line arguments is a command specifier. This 
can be one of `create` or `fork`. Or in fact, this can be other string literals 
too, because it can be dynamically extended by the Lamu configuration file.
The mechanism will be described later. When the string as the command specifier 
is none of `create` , `fork` nor other extended command names, the command 
specifier `default` is applied.
-->


