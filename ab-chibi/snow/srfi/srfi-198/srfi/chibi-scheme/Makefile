# -*- makefile-gmake -*-

.PHONY: test
.DEFAULT_GOAL := all

CHIBI_LOCAL ?= LD_LIBRARY_PATH="$(CHIBI_LOCATION_PATH):." DYLD_LIBRARY_PATH="$(CHIBI_LOCATION_PATH):." CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="$(CHIBI_LOCATION_PATH)/lib:./lib" $(CHIBI_LOCATION_PATH)/chibi-scheme$(EXE)

all:

########################################################################
# REPL

repl:
	$(CHIBI_LOCAL) -m "(srfi 198)"


########################################################################
# Tests

test:
	$(CHIBI_LOCAL) -m "(srfi 198 test)" -e "(run-tests)"

########################################################################
# Packaging

clean: clean-libs
	-$(RM) # $(META_FILES)
