#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec plt-r6rs ++path "$dir/.." "$dir/$1.sps"
