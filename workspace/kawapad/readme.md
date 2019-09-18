
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
| :----- | :----- | :----- |
|delete-previous|Backspace|  BACK_SPACE, CTRL-H  |
|kawapad-undo-action|Undo|  CTRL-Z  |
|kawapad-redo-action|Redo|  SHIFT-CTRL-Z  |
|kawapad-debug|Debug|  CTRL-ALT-BACK_QUOTE  |
|paste-from-clipboard|Paste|  CTRL-V  |
|caret-backward|caret-backward|  LEFT, CTRL-B  |
|caret-forward|caret-forward|  RIGHT, CTRL-F  |
|selection-backward|selection-backward|  SHIFT-LEFT, SHIFT-CTRL-B  |
|selection-forward|selection-forward|  SHIFT-RIGHT, SHIFT-CTRL-F  |
|selection-up|selection-up|  SHIFT-UP, SHIFT-CTRL-P  |
|selection-down|selection-down|  SHIFT-DOWN, SHIFT-CTRL-N  |
|caret-up|caret-up|  UP, CTRL-P  |
|caret-down|caret-down|  DOWN, CTRL-N  |
|page-up|page-up|    |
|page-down|page-down|    |
|kawapad-scroll-up|kawapad-scroll-up|  CTRL-UP  |
|kawapad-scroll-down|kawapad-scroll-down|  CTRL-DOWN  |
|kawapad-disable-content-assist|Disable Content Assist|  ESCAPE  |
|kawapad-enable-content-assist|Enable Content Assist|  CTRL-SPACE  |
|kawapad-reset|Reset the Environment|  SHIFT-CTRL-ALT-BACK_QUOTE  |
|kawapad-select-evaluate|Select and Evaluate|  CTRL-ENTER  |
|kawapad-evaluate|Select, Evaluate and Replace|  SHIFT-CTRL-ENTER  |
|kawapad-evaluate|Evaluate|  CTRL-E  |
|kawapad-run|Run|  CTRL-R  |
|kawapad-interrupt|Interrupt|  CTRL-K  |
|kawapad-simple-parenthesis-jump-left|Go to the Previous Parenthesis|  ALT-UP, ALT-P  |
|kawapad-simple-parenthesis-jump-right|Go to the Next Parenthesis|  ALT-DOWN, ALT-N  |
|kawapad-parenthesis-sel-jump-left|Lookup the Pair of Parenthesis on the Left and Select|  SHIFT-ALT-UP, SHIFT-ALT-P  |
|kawapad-parenthesis-sel-jump-right|Lookup the Pair of Parenthesis on the Right and Select|  SHIFT-ALT-DOWN, SHIFT-ALT-N  |
|kawapad-lispword-select-current|Select the Word on the Cursor.|    |
|kawapad-lisp-word-select-right|Select the Word on the Cursor.|  ALT-RIGHT, ALT-F  |
|kawapad-lisp-word-select-left|Select the Word on the Cursor.|  ALT-LEFT, ALT-B  |
|kawapad-lisp-word-swap-left|Swap the Word on the Left.|  SHIFT-ALT-LEFT, SHIFT-ALT-B  |
|kawapad-lisp-word-swap-right|Swap the Word on the Right.|  SHIFT-ALT-RIGHT, SHIFT-ALT-F  |
|kawapad-parenthesis-swap-left|Swap the Left Parentesis Pair|  SHIFT-CTRL-ALT-LEFT, SHIFT-CTRL-ALT-B  |
|kawapad-parenthesis-swap-right|Swap the Right Parentsis Pair|  SHIFT-CTRL-ALT-RIGHT, SHIFT-CTRL-ALT-F  |
|kawapad-parenthesis-expand-selection|Select Inside the Current Parentheses|  CTRL-ALT-UP, CTRL-ALT-P  |
|kawapad-select-left-parentheses|Select the Parentheses on the Left Side|  CTRL-ALT-LEFT, CTRL-ALT-B  |
|kawapad-select-right-parentheses|Select the Parentheses on the Left Side|  CTRL-ALT-RIGHT, CTRL-ALT-F  |
|kawapad-parenthesis-shrink-selection-dynamically|Deselect Inside the Current Parentheses|  CTRL-ALT-DOWN, CTRL-ALT-N  |
|kawapad-shift-indent-right|Shift Left|  TAB  |
|kawapad-shift-indent-left|Shift Right|  SHIFT-TAB  |
|kawapad-prettify|Correct Indentation|  CTRL-I  |
|kawapad-open-new|Open New|    |
|kawapad-open-file|Open|  CTRL-O  |
|kawapad-save-file|Save|  CTRL-S  |
|kawapad-save-file-as|Save as|  SHIFT-CTRL-S  |

