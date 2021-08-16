#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
CC=${CC:-clang}
CFLAGS=${CFLAGS:--Werror -Wall -Wextra -ansi -pedantic -fsanitize=address}
set -x
$CC $CFLAGS -o read-using-c read-using-c.c read-using-c-helpers.c
