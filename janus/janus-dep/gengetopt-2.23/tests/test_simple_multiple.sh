#! /bin/sh

set -ex

"$@" ./test_simple_multiple -s "first","second" --string "third"
