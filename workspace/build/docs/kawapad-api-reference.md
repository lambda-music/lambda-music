Kawapad API Reference
======================


ABOUT-INTRO
====================

#### SYNOPSIS ####
    (about-intro)

### DESCRIPTION ###
Welcome to Pulsar music sequencer! Pulsar music sequencer is a music sequencer
which collaboratively works with a powerful computer language Lisp Scheme. And this
frame itself is a powerful Lisp Scheme editor which is called Kawapad. In Lisp, all
commands are surrounded with a pair of parentheses. You can easily execute one of those
command by moving your cursor within the pair of parentheses and pressing CTRL+ENTER.

To show this help, execute \(help about-intro\). To show all available
procedures, execute \(help\) . To show help of a procedure, execute \(help
\[procedure-name\] \) .



--------------------------------------------------------

LOAD-FONT
====================

#### SYNOPSIS ####
    (load-font file-size::string font-size::number)::void

### DESCRIPTION ###
Set the main font of the editor. Kawapad can change its font-face.
||load-font|| loads a file from the filesystem and set it to the font-face of Kawapad.



--------------------------------------------------------

LOAD-FONT-UI
====================

#### SYNOPSIS ####
    (load-font-ui file-size::string font-size::number)::void

### DESCRIPTION ###
Set the main font of the ui. \_load-font-ui\_ loads a file from the specified
file and set it as the default font of the current ui.



--------------------------------------------------------

