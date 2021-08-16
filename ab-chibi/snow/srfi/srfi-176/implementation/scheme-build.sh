#!/bin/sh

#
# This is the build script for the Fantastic Scheme example. It
# generates the source file scheme_env_build.c. It then compiles and
# links Fantastic Scheme. CC, CFLAGS, and LFLAGS override the
# compiler, compiler flags, and linker flags, respectively. Any
# command line arguments given to the build script are shown as
# ./configure arguments to Fantastic Scheme, but have no other
# meaning.
#

set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"

## Default options

default_cc="cc"
default_cflags="-Wall -O2 -D SCHEME_UNIX"
default_lflags="-lm"

## Real options

CC="${CC:-$default_cc}"
CFLAGS="${CFLAGS:-$default_cflags}"
LFLAGS="${LFLAGS:-$default_lflags}"

## Generate scheme_env_build.c

clines() {
    sed -e 's/[^A-Za-z0-9_.=/+-]/\\&/g' -e 's/^/    "/' -e 's/$/",/'
    printf "    0"
}

build_date="$(date '+%Y-%m-%dT%H:%M%z' | sed 's@^.\{19\}@&:@')"
build_platform="$(uname -s)-$(uname -m)-$(basename "$CC")"

git_tag="$(git describe --tags --abbrev=0 2>/dev/null || true)"
git_branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
git_commit="$(git rev-parse --short HEAD 2>/dev/null || true)"
env_build_git_modified="$(git diff-index --name-only HEAD 2>/dev/null | head | clines)"
env_build_configure="$(echo "$@" | tr ' ' '\n' | clines)"
env_build_c_compile="$(echo "$CC" "$CFLAGS" | tr ' ' '\n' | clines)"
env_build_c_link="$(echo "$CC" "$LFLAGS" | tr ' ' '\n' | clines)"

cat >scheme_env_build.c <<EOF
// Generated from scratch at each build.

const char env_build_date[] = "$build_date";
const char env_build_platform[] = "$build_platform";
const char env_build_git_tag[] = "$git_tag";
const char env_build_git_branch[] = "$git_branch";
const char env_build_git_commit[] = "$git_commit";

const char *env_build_git_modified[] = {
$env_build_git_modified
};

const char *env_build_configure[] = {
$env_build_configure
};

const char *env_build_c_compile[] = {
$env_build_c_compile
};

const char *env_build_c_link[] = {
$env_build_c_link
};
EOF

## Compile and link Fantastic Scheme

set -x
$CC $CFLAGS -c scheme.c
$CC $CFLAGS -c scheme_env_build.c
$CC $CFLAGS -c scheme_env_release.c
$CC $LFLAGS -o scheme scheme.o scheme_env_build.o scheme_env_release.o
