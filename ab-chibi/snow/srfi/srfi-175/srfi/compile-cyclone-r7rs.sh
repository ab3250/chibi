#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
echo "Entering directory '$PWD'"
set -x
cyclone "srfi/175.sld"
cyclone "srfi/examples.scm"
cyclone "srfi/tests.scm"
