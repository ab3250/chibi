# Chibi Scheme example implementation of SRFI 198

Please see copyright notice in 198/COPYING.

The Makefile does not assume an installed Chibi Scheme is in any
particular location to build from, so you must set the environment
variable CHIBI_LOCATION_PATH to the desired location.  E.g., in bash:

export CHIBI_LOCATION_PATH=/usr/local/src/chibi-scheme

Or without modifying your environment, with gmake being Gnu Make or
some sort of alias to it, you could execute something like this using
bash to make the example SRFI implementation:

CHIBI_LOCATION_PATH=/home/src/chibi-scheme gmake

To run a REPL with all paths set correctly, and automatically loading
the library, with current directory of srfi/chibi-scheme off the top
repo directory, either do a "make repl", or execute something like:

LD_LIBRARY_PATH="/usr/local/src/chibi-scheme:." DYLD_LIBRARY_PATH="/usr/local/src/chibi-scheme:." CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="/usr/local/src/chibi-scheme/lib:./lib" /usr/local/src/chibi-scheme/chibi-scheme -m "(srfi 198)"

To run the tests, starting from the above location, either do a "make
test", or:

LD_LIBRARY_PATH="/usr/local/src/chibi-scheme:." DYLD_LIBRARY_PATH="/usr/local/src/chibi-scheme:." CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="/usr/local/src/chibi-scheme/lib:./lib" /usr/local/src/chibi-scheme/chibi-scheme -m "(srfi 198 test)" -e "(run-tests)"

Which run successfully on x86-64 Ubuntu 18.04 Linux kernel 4.15.0-106,
gcc v7.5.0, and x86-64 OpenBSD 6.5, clang v7.0.1.
