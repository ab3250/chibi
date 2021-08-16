# Copyright (C) 2002-2018 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.
#
# As a special exception to the GNU General Public License,
# this file may be distributed as part of a program that
# contains a configuration script generated by Autoconf, under
# the same distribution terms as the rest of that program.
#
# Generated by gnulib-tool.
#
# This file represents the specification of how gnulib-tool is used.
# It acts as a cache: It is written and read by gnulib-tool.
# In projects that use version control, this file is meant to be put under
# version control, like the configure.ac and various Makefile.am files.


# Specification in the form of a command-line invocation:
#   gnulib-tool --import --local-dir=lib/unistring/override --lib=libunistring --source-base=lib/unistring --m4-base=lib/unistring/m4 --doc-base=doc --tests-base=tests --aux-dir=build-aux --lgpl=3orGPLv2 --no-conditional-dependencies --libtool --macro-prefix=unistring unictype/category-all unictype/property-default-ignorable-code-point unictype/property-join-control unictype/property-not-a-character uninorm/nfc uninorm/nfkc uninorm/u16-normalize uninorm/u32-normalize uninorm/u8-normalize unistr/u16-to-u8 unistr/u32-to-u8 unistr/u8-check unistr/u8-to-u16 unistr/u8-to-u32

# Specification in the form of a few gnulib-tool.m4 macro invocations:
gl_LOCAL_DIR([lib/unistring/override])
gl_MODULES([
  unictype/category-all
  unictype/property-default-ignorable-code-point
  unictype/property-join-control
  unictype/property-not-a-character
  uninorm/nfc
  uninorm/nfkc
  uninorm/u16-normalize
  uninorm/u32-normalize
  uninorm/u8-normalize
  unistr/u16-to-u8
  unistr/u32-to-u8
  unistr/u8-check
  unistr/u8-to-u16
  unistr/u8-to-u32
])
gl_AVOID([])
gl_SOURCE_BASE([lib/unistring])
gl_M4_BASE([lib/unistring/m4])
gl_PO_BASE([])
gl_DOC_BASE([doc])
gl_TESTS_BASE([tests])
gl_LIB([libunistring])
gl_LGPL([3orGPLv2])
gl_MAKEFILE_NAME([])
gl_LIBTOOL
gl_MACRO_PREFIX([unistring])
gl_PO_DOMAIN([])
gl_WITNESS_C_MACRO([])
