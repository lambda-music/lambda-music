#!/bin/bash

shopt -s globstar

cd src.scheme

kawa -d ../bin.scheme/ -C ./**/*.scm
