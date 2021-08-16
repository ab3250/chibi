#!/bin/sh
set -eu
dir="$(dirname "$0")"
exec ypsilon --sitelib "$dir/.." "$dir/$1.sps"
