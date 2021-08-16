#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec ${CHEZ_SCHEME:-scheme} --libdirs "$dir/.." --program "$dir/$1.sps"
