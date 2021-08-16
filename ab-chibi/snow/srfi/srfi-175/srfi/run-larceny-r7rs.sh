#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec larceny -r7rs -A "$dir/.." "$dir/$1.scm"
