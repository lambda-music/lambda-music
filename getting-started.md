Getting Started
===============

# System Requirements #
Any operating systems that can run the following systems:
- Java 8
- JNA
- JACK Audio Connection Kit

Lamu has been developed and tested in Ubuntu 16.04. It runs under Windows 10
successfully. Running under OS X is still not tested; it should work as well 
operation systems.

Pulser uses following libraries :

- JNA-4.5.0
- JNAJACK-1.3.0
- KAWA-3.0
- JTattoo-1.6.11

These are statically linked and packed to `lamu.jar`.

# How to Install #

## Prerequisites ##
Lamu requires [Java8](https://www.java.com/en/download/) and [JACKAudio
Connection Kit](http://jackaudio.org/). These libraries should have properly 
installed on your environment before executing `lamu.jar`.

After the libraries are installed, download the `lamu.jar` from
[lamu.jar on the MASTER branch](https://github.com/lambda-music/lamu/blob/master/workspace/lamu/lamu.jar)

Lamu is packed into an executable JAR file and no installation is required.  
Just place the file any directory; preferably, it has better to be placed on a 
directory which is on the `$PATH` list.

# How to Run #

In a shell command prompt, execute

```bash
> java -jar /the-path-to-the-file/lamu.jar
```

In most platforms, you can also execute the application by double-clicking on
the file in your file-browser. 

## Command-line Parameter ##

Specifying a path to an arbitrary file as command-line parameter causes the 
Lamu editor to open the specified file.

```bash
> java jar lamu.jar any-scheme-program.scm
```

## Advanced Command-line Parameter ##
Lamu is composed by several components. And Lamu's command-line parameter can 
specify which of these components to be instantiated on at the start-up.

For further information, please read [Lamu Command-line Parameter 
Specification](./lamu-arguments.md)


### Execute Scheme Commands from Your Editors ###

If you are a VIM user, 
```VIM
:xmap <Return> :!curl -sSd "`cat`" http://localhost:8192/vim
```
this command effectively turns your VIM into a Scheme interactive editor. 
Select the specific text and hit your enter-key then the text will be executed 
in the Lamu application instance.

Lamu listens the port 8192 as a HTTP server, and executes any text which
comes via POST request from a client on localhost. It denies all requests from
hosts other than localhost.

Such kind of trick should easily be implemented in Emacs or other editors, too.

**Disclaimer**

Running Lamu in a public computer which has a network interface with any
global IP assigned or a running production server etc. causes great security
risks. Do not run Lamu in such situations.

