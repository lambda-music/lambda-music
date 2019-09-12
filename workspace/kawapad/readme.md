
   Kawapad a Scheme Editor
=============================

TODO write description for shortcut key.

ABOUT-INTRO
====================

#### SYNOPSIS ####
    (about-intro)

### DESCRIPTION ###
Welcome to Kawapad! Kawapad is a simple Lisp Scheme editor which can edit and
execute Scheme code on the fly. Kawapad includes Java implementation of a powerful
computer language Lisp Scheme. To show all available procedures, execute (help). To
show help of a procedure, execute (help [procedure-name] ) .



--------------------------------------------------------

LOAD-FONT
====================

#### SYNOPSIS ####
    (load-font file-size::string font-size::number)::void

### DESCRIPTION ###
Set the main font of the editor. Kawapad can change its font-face.
||load-font|| loads a file from the filesystem and set it to the font-face of Kawapad.



--------------------------------------------------------


| ID | Description | Keybind |
| :----: | :----: | :----- |
|kawapad-undo-action|Undo|  CTRL-Z  |
|kawapad-redo-action|Redo|  SHIFT-CTRL-Z  |
|kawapad-debug|Debug|  CTRL-ALT-BACK_QUOTE  |
|paste-from-clipboard|Paste|  CTRL-V  |
|caret-backward|caret-backward|  LEFT, CTRL-B  |
|caret-forward|caret-forward|  RIGHT, CTRL-F  |
|caret-up|caret-up|  UP, CTRL-P  |
|caret-down|caret-down|  DOWN, CTRL-N  |
|kawapad-scroll-up|kawapad-scroll-up|  CTRL-UP  |
|kawapad-scroll-down|kawapad-scroll-down|  CTRL-DOWN  |
|kawapad-disable-content-assist|Disable Content Assist|  ESCAPE  |
|kawapad-enable-content-assist|Enable Content Assist|  CTRL-SPACE  |
|kawapad-reset|Reset the Environment|    |
|kawapad-select-evaluate|Select and Evaluate|  CTRL-ENTER  |
|kawapad-evaluate|Select, Evaluate and Replace|  SHIFT-CTRL-ENTER  |
|kawapad-evaluate|Evaluate|  CTRL-E  |
|kawapad-run|Run|  CTRL-R  |
|kawapad-interrupt|Interrupt|  CTRL-K  |
|kawapad-simple-parenthesis-jump-left|Go to the Previous Parenthesis|  ALT-LEFT  |
|kawapad-simple-parenthesis-jump-right|Go to the Next Parenthesis|  ALT-RIGHT  |
|kawapad-parenthesis-sel-jump-left|Lookup the Pair of Parenthesis on the Left and Select|  SHIFT-CTRL-ALT-LEFT  |
|kawapad-parenthesis-sel-jump-right|Lookup the Pair of Parenthesis on the Right and Select|  SHIFT-CTRL-ALT-RIGHT  |
|kawapad-select-current-lisp-word|Select the Word on the Cursor.|  CTRL-ALT-UP  |
|kawapad-select-right-lisp-word|Select the Word on the Cursor.|  CTRL-ALT-RIGHT  |
|kawapad-select-left-lisp-word|Select the Word on the Cursor.|  CTRL-ALT-LEFT  |
|kawapad-expand-parenthesis-selection|Select Inside the Current Parentheses|  SHIFT-ALT-UP, SHIFT-ALT-P  |
|kawapad-select-left-parentheses|Select the Parentheses on the Left Side|  SHIFT-ALT-B, SHIFT-ALT-B  |
|kawapad-select-right-parentheses|Select the Parentheses on the Left Side|  SHIFT-ALT-F, SHIFT-ALT-F  |
|kawapad-select-parentheses-shrink|Select Parentheses Inside the Current Selection|  SHIFT-ALT-N, SHIFT-ALT-N  |
|kawapad-format-inc|Increase Indentation|  TAB  |
|kawapad-format-dec|Decrease Indentation|  SHIFT-TAB  |
|kawapad-prettify|Correct Indentation|  CTRL-I  |
|kawapad-open-new|Open New|  SHIFT-CTRL-N  |
|kawapad-open-file|Open|  CTRL-O  |
|kawapad-save-file|Save|  CTRL-S  |
|kawapad-save-file-as|Save as|  SHIFT-CTRL-S  |

