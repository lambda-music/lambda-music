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



