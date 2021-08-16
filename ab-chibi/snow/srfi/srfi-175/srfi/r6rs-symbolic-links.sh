#!/bin/sh
# R6RS Schemes do not have a universal convention for importing
# numerical library names. Set up some compatibility symlinks.
set -eu
cd "$(dirname "$0")"
set -x
ln -s "175.sls" "%3a175.sls"
ln -s "175.sls" ":175.sls"
