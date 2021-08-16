#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec gsi-script -:r7rs,search="$dir/.." "$dir/$1.scm"
