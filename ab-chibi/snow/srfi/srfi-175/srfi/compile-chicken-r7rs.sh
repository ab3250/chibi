#!/bin/sh
# You need the r7rs egg: `chicken-install r7rs`.
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
csc -R r7rs -prologue 175.sld examples.scm
csc -R r7rs -prologue 175.sld tests.scm
