#!/bin/bash

# This shell script was an experimental attempt to precompile Scheme files
# which was not actually used in the application. This remains here by a 
# historical reason.

shopt -s globstar

cd src.scheme

kawa -d ../bin.scheme/ -C ./**/*.scm

