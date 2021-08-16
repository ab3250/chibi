#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec vicare -I "$dir/.." "$dir/$1.sps"
