= Introduction
	Pulsar is a music sequencer program which allows users to write music as
	Lisp Scheme programs.

	In this system, musical notes and other informations are written as Scheme's
	association lists. 

	The musical notes can be dynamically generated association lists on-the-fly;
	a users can also interact with their programs to affect where the music go
	to.

	With this system, you can learn music and Lisp Scheme at same time.

	Pulsar is written in Java8 and Kawa, and processes MIDI data via with Jack
	Audio Connection Kit.  Pulsar has been developed on the Ubuntu 16.04.
	Pulsar is expected to run on any Unix-like systems and versions of Windows.

	This manual is for @value{NAME} version @value{VERSION}.

	= What's New
		The Pulsar Music Sequencer has just been released; this application is
		very new and has not been fully tested yet. 

		Please be careful to use this application.

	= Feature
		- Enables you to write pieces of music as Lisp Scheme program.
		- Built with Kawa a very powerful Lisp Scheme implementation.
		- Works with JACK Audio Connection Kit and can connect to any
		  synthesizer applications support JACK.

		- Includes Kawapad; Kawapad is an editor to edit Scheme program 
			- Kawapad can prettify Lisp code.
			- Execute a block of code on-the-fly.
			- Kawapad is extensible by Kawa-Scheme.

	= System Requirements
		Any operating systems that can run the following systems :
		- Java 8
		- JNA Java Native Access
		- Jack Audio Connection Kit

		Pulsar has been developed and tested in Ubuntu 16.04. A cursory
		experiment to run Pulsar in Windows 10 with Windows JACK was succeeded.
		It is still unknown if Pulsar can run in OS X and further experiments
		are needed.

	= Community 
		Currently Pulsar has no community. Currently Pulsar is developed by only
		one developer (me) and there is no user at all but the developer. The
		developer (me) strongly needs your help to improve the Pulsar.

= Getting Started
	= Installation


	= Dependency
		- JNA-4.5.0
		- JNAJACK-1.3.0
		- KAWA-3.0

	= Compilation
		= Compilation by Eclipse
		= Compilation by Ant

	= Execution
		Pulsar is simply an executable JAR file; before executing Pulsar, please
		make sure `java` is placed on a directory in `PATH`, and then execute 

			`java -jar /pulsar.jar`

		may start Pulsar.

		If java is properly configured, just double clicking the jar file can also
		start Pulsar.

		Currently Pulsar outputs its error messages into `stterr` it is necessary
		to execute it in a terminal screen to read its error messages.

		= Command Line Arguments

= Pulsar 
	= Scheme Notation

	= Application Model
		= Pulsar's Project File Model
			- Main file
			- Init file

	= Note-alist

	= API
		= Note Builder API
		= Pulser MIDI Controller API
		= Pulser GUI Controller API

	= Files

= Kawapad
	= Overview

	= API
		= Kawapad GUI Controller API

= Metro
	= Overview


; vim:textwidth=80 foldmethod=expr foldexpr=getline(v\:lnum)!\~'^[\\t\\s]\*\=\ \.\*\$' :
