#!/bin/sh
# You need to run compile-gerbil-r7rs.sh first.
set -eu
dir="$(dirname "$0")"
exec gxi --lang r7rs "$dir/$1.scm"
