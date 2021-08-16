# DO NOT EDIT! GENERATED AUTOMATICALLY!
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
# This file represents the compiled summary of the specification in
# gnulib-cache.m4. It lists the computed macro invocations that need
# to be invoked from configure.ac.
# In projects that use version control, this file can be treated like
# other built files.


# This macro should be invoked from ./configure.ac, in the section
# "Checks for programs", right after AC_PROG_CC, and certainly before
# any checks for libraries, header files, types and library functions.
AC_DEFUN([unistring_EARLY],
[
  m4_pattern_forbid([^gl_[A-Z]])dnl the gnulib macro namespace
  m4_pattern_allow([^gl_ES$])dnl a valid locale name
  m4_pattern_allow([^gl_LIBOBJS$])dnl a variable
  m4_pattern_allow([^gl_LTLIBOBJS$])dnl a variable

  # Pre-early section.
  AC_REQUIRE([gl_PROG_AR_RANLIB])

  AC_REQUIRE([AM_PROG_CC_C_O])
  # Code from module absolute-header:
  # Code from module array-mergesort:
  # Code from module gperf:
  # Code from module include_next:
  # Code from module inline:
  # Code from module limits-h:
  # Code from module multiarch:
  # Code from module snippet/unused-parameter:
  # Code from module ssize_t:
  # Code from module stdbool:
  # Code from module stdint:
  # Code from module sys_types:
  # Code from module unictype/base:
  # Code from module unictype/category-C:
  # Code from module unictype/category-Cc:
  # Code from module unictype/category-Cf:
  # Code from module unictype/category-Cn:
  # Code from module unictype/category-Co:
  # Code from module unictype/category-Cs:
  # Code from module unictype/category-L:
  # Code from module unictype/category-LC:
  # Code from module unictype/category-Ll:
  # Code from module unictype/category-Lm:
  # Code from module unictype/category-Lo:
  # Code from module unictype/category-Lt:
  # Code from module unictype/category-Lu:
  # Code from module unictype/category-M:
  # Code from module unictype/category-Mc:
  # Code from module unictype/category-Me:
  # Code from module unictype/category-Mn:
  # Code from module unictype/category-N:
  # Code from module unictype/category-Nd:
  # Code from module unictype/category-Nl:
  # Code from module unictype/category-No:
  # Code from module unictype/category-P:
  # Code from module unictype/category-Pc:
  # Code from module unictype/category-Pd:
  # Code from module unictype/category-Pe:
  # Code from module unictype/category-Pf:
  # Code from module unictype/category-Pi:
  # Code from module unictype/category-Po:
  # Code from module unictype/category-Ps:
  # Code from module unictype/category-S:
  # Code from module unictype/category-Sc:
  # Code from module unictype/category-Sk:
  # Code from module unictype/category-Sm:
  # Code from module unictype/category-So:
  # Code from module unictype/category-Z:
  # Code from module unictype/category-Zl:
  # Code from module unictype/category-Zp:
  # Code from module unictype/category-Zs:
  # Code from module unictype/category-all:
  # Code from module unictype/category-and:
  # Code from module unictype/category-and-not:
  # Code from module unictype/category-byname:
  # Code from module unictype/category-longname:
  # Code from module unictype/category-name:
  # Code from module unictype/category-none:
  # Code from module unictype/category-of:
  # Code from module unictype/category-or:
  # Code from module unictype/category-test:
  # Code from module unictype/category-test-withtable:
  # Code from module unictype/combining-class:
  # Code from module unictype/property-default-ignorable-code-point:
  # Code from module unictype/property-join-control:
  # Code from module unictype/property-not-a-character:
  # Code from module uninorm/base:
  # Code from module uninorm/canonical-decomposition:
  # Code from module uninorm/compat-decomposition:
  # Code from module uninorm/composition:
  # Code from module uninorm/decompose-internal:
  # Code from module uninorm/decomposition:
  # Code from module uninorm/decomposition-table:
  # Code from module uninorm/nfc:
  # Code from module uninorm/nfd:
  # Code from module uninorm/nfkc:
  # Code from module uninorm/nfkd:
  # Code from module uninorm/u16-normalize:
  # Code from module uninorm/u32-normalize:
  # Code from module uninorm/u8-normalize:
  # Code from module unistr/base:
  # Code from module unistr/u16-cpy:
  # Code from module unistr/u16-mbtouc-unsafe:
  # Code from module unistr/u16-mbtoucr:
  # Code from module unistr/u16-to-u8:
  # Code from module unistr/u16-uctomb:
  # Code from module unistr/u32-cpy:
  # Code from module unistr/u32-mbtouc-unsafe:
  # Code from module unistr/u32-to-u8:
  # Code from module unistr/u32-uctomb:
  # Code from module unistr/u8-check:
  # Code from module unistr/u8-cpy:
  # Code from module unistr/u8-mbtouc-unsafe:
  # Code from module unistr/u8-mbtoucr:
  # Code from module unistr/u8-to-u16:
  # Code from module unistr/u8-to-u32:
  # Code from module unistr/u8-uctomb:
  # Code from module unitypes:
])

# This macro should be invoked from ./configure.ac, in the section
# "Check for header files, types and library functions".
AC_DEFUN([unistring_INIT],
[
  AM_CONDITIONAL([GL_COND_LIBTOOL], [true])
  gl_cond_libtool=true
  gl_m4_base='lib/unistring/m4'
  m4_pushdef([AC_LIBOBJ], m4_defn([unistring_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([unistring_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([unistring_LIBSOURCES]))
  m4_pushdef([unistring_LIBSOURCES_LIST], [])
  m4_pushdef([unistring_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='lib/unistring'
  gl_INLINE
  gl_LIMITS_H
  gl_MULTIARCH
  gt_TYPE_SSIZE_T
  AM_STDBOOL_H
  gl_STDINT_H
  gl_SYS_TYPES_H
  AC_PROG_MKDIR_P
  gl_LIBUNISTRING_LIBHEADER([0.9.4], [unictype.h])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-C])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Cc])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Cf])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Cn])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Co])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Cs])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-L])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-LC])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Ll])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Lm])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Lo])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Lt])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Lu])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-M])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Mc])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Me])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Mn])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-N])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Nd])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Nl])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-No])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-P])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Pc])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Pd])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Pe])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Pf])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Pi])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Po])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Ps])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-S])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Sc])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Sk])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Sm])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-So])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Z])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Zl])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Zp])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-Zs])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-and])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-and-not])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-byname])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-longname])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-name])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-none])
  AC_REQUIRE([AC_C_INLINE])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-of])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-or])
  AC_REQUIRE([AC_C_INLINE])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/category-test])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/combining-class])
  AC_REQUIRE([AC_C_INLINE])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/property-default-ignorable-code-point])
  AC_REQUIRE([AC_C_INLINE])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/property-join-control])
  AC_REQUIRE([AC_C_INLINE])
  gl_LIBUNISTRING_MODULE([0.9.8], [unictype/property-not-a-character])
  gl_LIBUNISTRING_LIBHEADER([0.9.4], [uninorm.h])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/canonical-decomposition])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/composition])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/decomposition])
  AC_REQUIRE([AC_C_INLINE])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/nfc])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/nfd])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/nfkc])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/nfkd])
  gl_MODULE_INDICATOR_FOR_TESTS([uninorm/u16-normalize])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/u16-normalize])
  gl_MODULE_INDICATOR_FOR_TESTS([uninorm/u32-normalize])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/u32-normalize])
  gl_MODULE_INDICATOR_FOR_TESTS([uninorm/u8-normalize])
  gl_LIBUNISTRING_MODULE([0.9.8], [uninorm/u8-normalize])
  gl_LIBUNISTRING_LIBHEADER([0.9.4], [unistr.h])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u16-cpy])
  gl_MODULE_INDICATOR([unistr/u16-mbtouc-unsafe])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u16-mbtouc-unsafe])
  gl_MODULE_INDICATOR([unistr/u16-mbtoucr])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u16-mbtoucr])
  gl_LIBUNISTRING_MODULE([0.9.3], [unistr/u16-to-u8])
  gl_MODULE_INDICATOR([unistr/u16-uctomb])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u16-uctomb])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u32-cpy])
  gl_MODULE_INDICATOR([unistr/u32-mbtouc-unsafe])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u32-mbtouc-unsafe])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u32-to-u8])
  gl_MODULE_INDICATOR([unistr/u32-uctomb])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u32-uctomb])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u8-check])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u8-cpy])
  gl_MODULE_INDICATOR([unistr/u8-mbtouc-unsafe])
  gl_LIBUNISTRING_MODULE([0.9.4], [unistr/u8-mbtouc-unsafe])
  gl_MODULE_INDICATOR([unistr/u8-mbtoucr])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u8-mbtoucr])
  gl_LIBUNISTRING_MODULE([0.9.3], [unistr/u8-to-u16])
  gl_LIBUNISTRING_MODULE([0.9.3], [unistr/u8-to-u32])
  gl_MODULE_INDICATOR([unistr/u8-uctomb])
  gl_LIBUNISTRING_MODULE([0.9], [unistr/u8-uctomb])
  gl_LIBUNISTRING_LIBHEADER([0.9.4], [unitypes.h])
  # End of code from modules
  m4_ifval(unistring_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([unistring_LIBSOURCES_DIR])[ ||
      for gl_file in ]unistring_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([unistring_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([unistring_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([unistring_LIBSOURCES_DIR])
  m4_popdef([unistring_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    unistring_libobjs=
    unistring_ltlibobjs=
    if test -n "$unistring_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $unistring_LIBOBJS; do echo "$i"; done | sed -e "$sed_drop_objext" | sort | uniq`; do
        unistring_libobjs="$unistring_libobjs $i.$ac_objext"
        unistring_ltlibobjs="$unistring_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([unistring_LIBOBJS], [$unistring_libobjs])
    AC_SUBST([unistring_LTLIBOBJS], [$unistring_ltlibobjs])
  ])
  gltests_libdeps=
  gltests_ltlibdeps=
  m4_pushdef([AC_LIBOBJ], m4_defn([unistringtests_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([unistringtests_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([unistringtests_LIBSOURCES]))
  m4_pushdef([unistringtests_LIBSOURCES_LIST], [])
  m4_pushdef([unistringtests_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='tests'
changequote(,)dnl
  unistringtests_WITNESS=IN_`echo "${PACKAGE-$PACKAGE_TARNAME}" | LC_ALL=C tr abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ | LC_ALL=C sed -e 's/[^A-Z0-9_]/_/g'`_GNULIB_TESTS
changequote([, ])dnl
  AC_SUBST([unistringtests_WITNESS])
  gl_module_indicator_condition=$unistringtests_WITNESS
  m4_pushdef([gl_MODULE_INDICATOR_CONDITION], [$gl_module_indicator_condition])
  m4_popdef([gl_MODULE_INDICATOR_CONDITION])
  m4_ifval(unistringtests_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([unistringtests_LIBSOURCES_DIR])[ ||
      for gl_file in ]unistringtests_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([unistringtests_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([unistringtests_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([unistringtests_LIBSOURCES_DIR])
  m4_popdef([unistringtests_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    unistringtests_libobjs=
    unistringtests_ltlibobjs=
    if test -n "$unistringtests_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $unistringtests_LIBOBJS; do echo "$i"; done | sed -e "$sed_drop_objext" | sort | uniq`; do
        unistringtests_libobjs="$unistringtests_libobjs $i.$ac_objext"
        unistringtests_ltlibobjs="$unistringtests_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([unistringtests_LIBOBJS], [$unistringtests_libobjs])
    AC_SUBST([unistringtests_LTLIBOBJS], [$unistringtests_ltlibobjs])
  ])
])

# Like AC_LIBOBJ, except that the module name goes
# into unistring_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([unistring_LIBOBJ], [
  AS_LITERAL_IF([$1], [unistring_LIBSOURCES([$1.c])])dnl
  unistring_LIBOBJS="$unistring_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into unistring_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([unistring_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [unistring_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([unistring_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([unistring_LIBSOURCES_DIR], [lib/unistring])
      m4_append([unistring_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# Like AC_LIBOBJ, except that the module name goes
# into unistringtests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([unistringtests_LIBOBJ], [
  AS_LITERAL_IF([$1], [unistringtests_LIBSOURCES([$1.c])])dnl
  unistringtests_LIBOBJS="$unistringtests_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into unistringtests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([unistringtests_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [unistringtests_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([unistringtests_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([unistringtests_LIBSOURCES_DIR], [tests])
      m4_append([unistringtests_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# This macro records the list of files which have been installed by
# gnulib-tool and may be removed by future gnulib-tool invocations.
AC_DEFUN([unistring_FILE_LIST], [
  lib/array-mergesort.h
  lib/limits.in.h
  lib/stdbool.in.h
  lib/stdint.in.h
  lib/sys_types.in.h
  lib/unictype.in.h
  lib/unictype/bitmap.h
  lib/unictype/categ_C.c
  lib/unictype/categ_C.h
  lib/unictype/categ_Cc.c
  lib/unictype/categ_Cc.h
  lib/unictype/categ_Cf.c
  lib/unictype/categ_Cf.h
  lib/unictype/categ_Cn.c
  lib/unictype/categ_Cn.h
  lib/unictype/categ_Co.c
  lib/unictype/categ_Co.h
  lib/unictype/categ_Cs.c
  lib/unictype/categ_Cs.h
  lib/unictype/categ_L.c
  lib/unictype/categ_L.h
  lib/unictype/categ_LC.c
  lib/unictype/categ_LC.h
  lib/unictype/categ_Ll.c
  lib/unictype/categ_Ll.h
  lib/unictype/categ_Lm.c
  lib/unictype/categ_Lm.h
  lib/unictype/categ_Lo.c
  lib/unictype/categ_Lo.h
  lib/unictype/categ_Lt.c
  lib/unictype/categ_Lt.h
  lib/unictype/categ_Lu.c
  lib/unictype/categ_Lu.h
  lib/unictype/categ_M.c
  lib/unictype/categ_M.h
  lib/unictype/categ_Mc.c
  lib/unictype/categ_Mc.h
  lib/unictype/categ_Me.c
  lib/unictype/categ_Me.h
  lib/unictype/categ_Mn.c
  lib/unictype/categ_Mn.h
  lib/unictype/categ_N.c
  lib/unictype/categ_N.h
  lib/unictype/categ_Nd.c
  lib/unictype/categ_Nd.h
  lib/unictype/categ_Nl.c
  lib/unictype/categ_Nl.h
  lib/unictype/categ_No.c
  lib/unictype/categ_No.h
  lib/unictype/categ_P.c
  lib/unictype/categ_P.h
  lib/unictype/categ_Pc.c
  lib/unictype/categ_Pc.h
  lib/unictype/categ_Pd.c
  lib/unictype/categ_Pd.h
  lib/unictype/categ_Pe.c
  lib/unictype/categ_Pe.h
  lib/unictype/categ_Pf.c
  lib/unictype/categ_Pf.h
  lib/unictype/categ_Pi.c
  lib/unictype/categ_Pi.h
  lib/unictype/categ_Po.c
  lib/unictype/categ_Po.h
  lib/unictype/categ_Ps.c
  lib/unictype/categ_Ps.h
  lib/unictype/categ_S.c
  lib/unictype/categ_S.h
  lib/unictype/categ_Sc.c
  lib/unictype/categ_Sc.h
  lib/unictype/categ_Sk.c
  lib/unictype/categ_Sk.h
  lib/unictype/categ_Sm.c
  lib/unictype/categ_Sm.h
  lib/unictype/categ_So.c
  lib/unictype/categ_So.h
  lib/unictype/categ_Z.c
  lib/unictype/categ_Z.h
  lib/unictype/categ_Zl.c
  lib/unictype/categ_Zl.h
  lib/unictype/categ_Zp.c
  lib/unictype/categ_Zp.h
  lib/unictype/categ_Zs.c
  lib/unictype/categ_Zs.h
  lib/unictype/categ_and.c
  lib/unictype/categ_and_not.c
  lib/unictype/categ_byname.c
  lib/unictype/categ_byname.gperf
  lib/unictype/categ_longname.c
  lib/unictype/categ_name.c
  lib/unictype/categ_none.c
  lib/unictype/categ_of.c
  lib/unictype/categ_of.h
  lib/unictype/categ_or.c
  lib/unictype/categ_test.c
  lib/unictype/combiningclass.c
  lib/unictype/combiningclass.h
  lib/unictype/pr_default_ignorable_code_point.c
  lib/unictype/pr_default_ignorable_code_point.h
  lib/unictype/pr_join_control.c
  lib/unictype/pr_join_control.h
  lib/unictype/pr_not_a_character.c
  lib/unictype/pr_not_a_character.h
  lib/uninorm.in.h
  lib/uninorm/canonical-decomposition.c
  lib/uninorm/compat-decomposition.c
  lib/uninorm/composition-table.gperf
  lib/uninorm/composition.c
  lib/uninorm/decompose-internal.c
  lib/uninorm/decompose-internal.h
  lib/uninorm/decomposition-table.c
  lib/uninorm/decomposition-table.h
  lib/uninorm/decomposition-table1.h
  lib/uninorm/decomposition-table2.h
  lib/uninorm/decomposition.c
  lib/uninorm/nfc.c
  lib/uninorm/nfd.c
  lib/uninorm/nfkc.c
  lib/uninorm/nfkd.c
  lib/uninorm/normalize-internal.h
  lib/uninorm/u-normalize-internal.h
  lib/uninorm/u16-normalize.c
  lib/uninorm/u32-normalize.c
  lib/uninorm/u8-normalize.c
  lib/unistr.in.h
  lib/unistr/u-cpy.h
  lib/unistr/u16-cpy.c
  lib/unistr/u16-mbtouc-unsafe-aux.c
  lib/unistr/u16-mbtouc-unsafe.c
  lib/unistr/u16-mbtoucr.c
  lib/unistr/u16-to-u8.c
  lib/unistr/u16-uctomb-aux.c
  lib/unistr/u16-uctomb.c
  lib/unistr/u32-cpy.c
  lib/unistr/u32-mbtouc-unsafe.c
  lib/unistr/u32-to-u8.c
  lib/unistr/u32-uctomb.c
  lib/unistr/u8-check.c
  lib/unistr/u8-cpy.c
  lib/unistr/u8-mbtouc-unsafe-aux.c
  lib/unistr/u8-mbtouc-unsafe.c
  lib/unistr/u8-mbtoucr.c
  lib/unistr/u8-to-u16.c
  lib/unistr/u8-to-u32.c
  lib/unistr/u8-uctomb-aux.c
  lib/unistr/u8-uctomb.c
  lib/unitypes.in.h
  lib/unused-parameter.h
  m4/00gnulib.m4
  m4/absolute-header.m4
  m4/gnulib-common.m4
  m4/include_next.m4
  m4/inline.m4
  m4/libunistring-base.m4
  m4/limits-h.m4
  m4/longlong.m4
  m4/multiarch.m4
  m4/off_t.m4
  m4/ssize_t.m4
  m4/stdbool.m4
  m4/stdint.m4
  m4/sys_types_h.m4
  m4/wint_t.m4
])
