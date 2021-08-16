#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec kawa --r7rs -Dkawa.import.path="$(cd "$dir/.." && pwd)/*.sld" "$dir/$1.scm"
