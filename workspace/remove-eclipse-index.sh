#!/bin/bash

TARGET_DIR="./.metadata/.plugins/org.eclipse.jdt.core"

mv "$TARGET_DIR" "${TARGET_DIR}--(`date --rfc-2822`)"
