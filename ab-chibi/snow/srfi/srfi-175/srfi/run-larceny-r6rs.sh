#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec larceny -r6rs -A "$dir/.." "$dir/$1.sps"
