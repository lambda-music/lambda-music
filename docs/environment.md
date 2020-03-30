Lamu Environment Variable Specification
=======================================

# Lamu Environment Variable to System Property Translation #
Every environment variable which starts with "LAMU\_" will be translated into a 
system property when Lamu starts. The background of this behavior is designed 
is to be used with [binfmt\_misc kernel 
module](https://en.wikipedia.org/wiki/Binfmt_misc).

The binfmt is great and it drastically simplifies the execution process of JAR.  
The major drawback of the usage of binfmt is the lack of the way to specify 
system properties which can usually be specified by [-D][-D] options.

[-D]:(https://docs.oracle.com/en/java/javase/13/docs/specs/man/java.html)

This leads you to miss the way to setting logging options.

Lamu has a featured function to take countermeasure to this; that is
Lamu Environmental Variable to System Property Translation.

Every environment variable which starts with "LAMU\_" will be translated into a 
system property; for example, an environment variable `LAMU_HELLO_WORLD` will 
be translated into `lamu.hello-world`.

1. The first underscore letter on an environment variable will be translated 
   into `.` a dot.
2. The other underscore letters on the environment variable will be translated 
   into `-` a minus sign.


# System Properties that Lamu Accepts #

- `lamu.enable-lamu-formatter`
- `lamu.logging-properties`

## `lamu.enable-lamu-formatter` ##

This can be specified by the environment variable which name is 
`LAMU_ENABLE_LAMU_FORMATTER`. When this property is set, Lamu sets up
the default logger formatter for the system.

##`lamu.logging-properties`##
This can be specified by the environment variable which name is 
`LAMU_LOGGING_PROPERTIES`. When this property is set, Lamu loads 
logging.properties files from the path specified in the environment
variable.

That is, Lamu forwards the value to [java.util.logging.config.file][A] system 
property value.

[A]:(https://docs.oracle.com/javase/7/docs/api/java/util/logging/LogManager.html)


