#! /bin/sh

set -ex

"$@" ./test_unamed --help | grep -F '[FILE]...'
