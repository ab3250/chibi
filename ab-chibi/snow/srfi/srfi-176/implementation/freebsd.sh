#!/bin/sh
set -eu
cd "$(dirname "$0")"
input="$(cat)"

# pkg install busybox heirloom mawk sbase toybox

./read-using-c-build.sh
echo "$input" | ./read-using-c >out-c

# heirloom
export AWK="/usr/local/heirloom/usr/5bin/nawk"
export GREP="/usr/local/heirloom/usr/5bin/grep"
export SED="/usr/local/heirloom/usr/5bin/sed"
echo "$input" | ./read-using-grep-sed.sh >out-grep-sed-heirloom
echo "$input" | ./read-using-awk.sh >out-awk-heirloom

# busybox
export AWK="busybox awk"
export GREP="busybox grep"
export SED="busybox sed"
echo "$input" | ./read-using-grep-sed.sh >out-grep-sed-busybox
echo "$input" | ./read-using-awk.sh >out-awk-busybox

# sbase
export GREP="/usr/local/sbase/bin/grep"
export SED="/usr/local/sbase/bin/sed"
echo "$input" | ./read-using-grep-sed.sh >out-grep-sed-suckless

# toybox
export GREP="toybox grep"
export SED="toybox sed"
echo "$input" | ./read-using-grep-sed.sh >out-grep-sed-toybox

# mawk
export AWK=mawk
echo "$input" | ./read-using-awk.sh >out-awk-mawk

# one true awk
export AWK=nawk
echo "$input" | ./read-using-awk.sh >out-awk-nawk

# one true awk
export AWK=awk
echo "$input" | ./read-using-awk.sh >out-awk-awk

# Check that all outputs are the same; if they are, nothing is shown.
first="out-awk-awk"
others="out-awk-busybox out-awk-heirloom out-awk-mawk out-awk-nawk out-c out-grep-sed-busybox out-grep-sed-heirloom out-grep-sed-suckless out-grep-sed-toybox"
for other in $others; do diff -u "$first" "$other"; done
