MAKE-PAGE
====================

#### SYNOPSIS ####
    (make-page content::string)::kawapad-page

### DESCRIPTION ###
make-page makes the passed value into ||kawapad-page|| object. When an
expression is evaluated in KawaPad, the result value is displayed on the current editor.
When the result value is a ||kawapad-page|| object, the value is displayed in a
special way; when the KawaPad system detect the result value is a ||kawapad-page||, the
editor expands the current selection to the outer-most parentheses and replace the
region with the result value. This enables it to use KawaPad as a dynamic Hypertext
editor.

The make-page procedure convert the passed value into the kawapad-page object
in order to activate the special display function of KawaPad.



--------------------------------------------------------

HELP!
====================

#### SYNOPSIS ####
    (help!)::string

### DESCRIPTION ###
is a procedure to execute when the user needs something which calms you down.
When this procedure is called, this procedure will return a message which tries to
calm the user down. Any argument specified to this procedure will be silently
ignored.This procedure is deliberately defined as a joke and has by no means effect to the
current system state nor any other related elements. See (help about-main).



--------------------------------------------------------

HELP
====================

#### SYNOPSIS ####
    (help|he [query::'procs|'notes|'all|procedure='all])::string|list

### DESCRIPTION ###
is a procedure to show the description of a specified procedure. When a
reference to a procedure is passed, ||help|| returns the description of the the
procedure.

If no procedure is specified, it returns a list that contains all procedures
which description is available. Pass a special keyword 'all to get a symbol list of
all procedures which are available for this command. Pass 'procs to get all
available procedures. Pass 'notes to get all available notation types.



--------------------------------------------------------

MAKE-HELP
====================

#### SYNOPSIS ####
    (make-help target::procedure content::(list cons ...))::void

### DESCRIPTION ###
||make-help|| registers a reference manual for a procedure on the Pulsar
documentation system. The ||target|| argument is the reference of the target procedure.The
||content|| argument is the content of the reference manual. The value is an association
list contains various data.

    (make-help   target-proc               '((names "foo-bar" "fb") 
               (params
                 ("param-name" "param-type" "default-value or #f if no-default" "#t if variable-length" "description") 
                    ...
                 )
                (returns "return-type" )
                (short-description "description" )
                (long-description  "description" )
              )

The ||name|| field contains names of the procedure. In Pulsar, the most
procedures have multiple names. The first element of this list is its 'long name' which
should be the canonical name for the procedure. And the others are its aliases. If the
procedure have no alias, then the list will have only one element. The list must have at
least one element.

The ||params|| field contains information of parameters. The field contains a
list per a parameter.

The ||short-description|| field contains a string value of its short
description. The ||long-description|| field contains a string value of its long description.



--------------------------------------------------------

HELP-MARKDOWN
====================

#### SYNOPSIS ####
    (help-markdown [type::string='procs])::string

### DESCRIPTION ###
is a procedure to execute when the user needs something which calms you down.
When this procedure is called, this procedure will return a message which tries to
calm the user down. Any argument specified to this procedure will be silently
ignored.This procedure is deliberately defined as a joke and has by no means effect to the
current system state nor any other related elements. See (help about-main).



--------------------------------------------------------

NOTATION
====================

#### SYNOPSIS ####
    (notation|n [arg::any]...)::notation

### DESCRIPTION ###
notation creates a notation or a list of notations. This procedure is a tool
which is designed to ease the process of building notation lists.



--------------------------------------------------------


