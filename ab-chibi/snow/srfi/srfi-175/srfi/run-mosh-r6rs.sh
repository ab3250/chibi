#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec mosh --loadpath "$dir/.." "$dir/$1.sps"
