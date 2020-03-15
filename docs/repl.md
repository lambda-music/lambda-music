Lamu REPL Processor
======================

# Design Goal #
Lamu REPL should be able to interact directly with humans and also be able to 
work with standard-input/output in order to automate the controlling of Lamu's 
control-flow.

# Overview #
The commands and other data are passed from the standard-input as text-data.  
The data is processed line-wise. The processor has a buffer to store the 
text-data. A line which is prefixed with specific characters is treated as a 
command-line;  otherwise the line is accumulated in the buffer.

The default command-line prefix is `;lamu:`. This can be changed by a command 
which is described later.

The command-line can take an argument. The command and the argument are 
separated by blank characters. The string literal after the blank characters is 
treated as an argument.

# Available Commands #

- hello
- alive?
- prefix
- ?
- help
- bye
- quit
- load
- save
- delete
- echo
- exec
- list
- clear
- show
- error

# `hello` #
Returns a simple greeting. This command has no effect to the current state.
```
;lamu:hello
```
# `alive?` #
This always returns #t unless the application is malfunctioning.

# `prefix` #
Change the command-line prefix.

```
;lamu:prefix ;lambda-music:
```
After executing above, you can execute a command by the following.

```
;lambda-music:hello
```

# `help` #
Returns a list of available command names.

# `?` #
Same as `help`

# `bye` #
End the program.

# `quit` #
End the program.

# `show` #
Returns the content of the buffer.

# `clear` #
Clear the buffer.

# `exec` #
Executes the content of the buffer. After executing this command, the buffer 
will be cleared.

# `save` #
Save the content of the buffer to the specified buffer-storage. After executing 
this command, the buffer will be cleared.

# `load` #
Load data from the specified buffer-storage and set it to the buffer.

# `delete` #
Delete the specified buffer-storage.

# `echo` #
Output the content of the buffer. After executing this command, the buffer will 
be cleared.

# `list` #
Output the list of the available buffer-storages. 

# `error` #
Returns an error message with the specified reason.


