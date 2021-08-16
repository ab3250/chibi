#!/bin/bash
set -eu
cd "$(dirname "$0")/.."
exec -a scheme-script loko "srfi/$1.sps"
