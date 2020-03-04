Command-line Parameter Reference
=================================

Lamu consists several components. It is able to be specified which components 
should be instantiated at the runtime. Lamu's command-line parameter specifies 
the component specs.

Lamu is also be able to run in client-server model. A Java editor can sometimes 
be a big burden for the garbage collection; A editor can be separately executed 
in another JVM from the JVM which runs Scheme engine. The command-line supports
such case.

# Syntax #

```
[type]
```





Lamu has two interfaces : HTTP interface and window interface.
```bash
> java jar lamu.jar --no-gui 
```
This disables the window interface.


```bash
> java jar lamu.jar --no-http
```
This disables the HTTP interface.

```bash
> java jar lamu.jar [filename]
```
Otherwise every argument is taken as filename. Though, only the first argument
is applied; other arguments are silently ignored. Note that when //--no-gui//
is specified, no filename is applied since there is no editor to edit in that
case.

### How to Run (Advanced) ###

It is very interesting that Kawa can invoke every method on the fly; that is
you do not have to specify the startup class when you start the JVM. Add the
path of//lamu.jar// to your CLASSPATH then execute Kawa's REPL.

```bash
CLASSPATH="$CLASSPATH:/path-to-lamu-dir/lamu.jar" kawa
```
Then, execute the following command in Kawa's REPL.
```scheme
(lamu.LamuApplication:main (java.lang.String[]))
```
This also starts Lamu sequencer. This may give you some possibility to
control of the application more precisely.

