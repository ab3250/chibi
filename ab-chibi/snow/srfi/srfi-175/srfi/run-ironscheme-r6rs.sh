#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec ironscheme -I "$dir/.." "$dir/$1.sps"
