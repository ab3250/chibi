#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
export IKARUS_LIBRARY_PATH=$PWD
exec ikarus --r6rs-script "srfi/$1.sps"
