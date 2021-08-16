#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
echo "Entering directory '$PWD'"
set -x
gsc-script -:r7rs -c -module-ref srfi/175        srfi/175.sld
gsc-script -:r7rs -c -module-ref srfi/examples . srfi/examples.scm
gsc-script -:r7rs -c -module-ref srfi/tests    . srfi/tests.scm
gsc-script -:r7rs -exe -nopreload srfi/175.c srfi/examples.c
gsc-script -:r7rs -exe -nopreload srfi/175.c srfi/tests.c
rm srfi/175.c srfi/examples.c srfi/tests.c
