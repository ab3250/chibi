#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec sagittarius -r 6 -A "$dir/.." "$dir/$1.sps"
