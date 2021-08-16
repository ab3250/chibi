#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec guile -L "$dir/.." -x .sls "$dir/$1.sps"
