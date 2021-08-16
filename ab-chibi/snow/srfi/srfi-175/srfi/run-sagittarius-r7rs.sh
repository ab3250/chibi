#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec sagittarius -r 7 -A "$dir/.." "$dir/$1.scm"
