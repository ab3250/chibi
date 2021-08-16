#!/bin/sh
set -eu
echo "Local date only:"
date '+%Y-%m-%d'
echo
echo "Local date and time:"
date '+%Y-%m-%dT%H:%M%z' | sed 's@^.\{19\}@&:@'
echo
echo "UTC date and time:"
date -u '+%Y-%m-%dT%H:%MZ'
