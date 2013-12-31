#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
LT_PREREQ([2.4.2])
AC_INIT([SciLisp], [0.01], [BUG-REPORT-ADDRESS])
AC_CONFIG_SRCDIR([scilisp.h])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign -Wall -Wno-gnu])
AM_PROG_AR
LT_INIT
# Checks for programs.
AC_PROG_AWK
AC_PROG_LEX
AC_PROG_CC
AC_PROG_CC_C99
AC_PROG_INSTALL
AC_PROG_LN_S
AC_CHECK_PROG(HAVE_EMACS,emacs,yes)


# Checks for libraries.
AC_CHECK_LIB([gc], [GC_malloc])
AC_CHECK_LIB([cord], [CORD_cat])
AC_CHECK_LIB([clang], [clang_visitChildren])
AC_CHECK_LIB([dl], [dlsym])
AC_CHECK_LIB([gmp], [__gmpz_add],,
                    [AC_MSG_ERROR([Error, required library gmp not found])])
AC_CHECK_LIB([m], [cos])
AC_CHECK_LIB([mpfr], [mpfr_init2],,
                    [AC_MSG_ERROR([Error, required library mpfr not found])])
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([readline], [readline],
                         [AC_DEFINE([HAVE_READLINE],[],[Define if Readline found]) LIBS="$LIBS -lreadline"],
                         [AC_MSG_WARN([Readline not found])])
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([fcntl.h float.h inttypes.h langinfo.h libintl.h limits.h locale.h stddef.h stdint.h stdlib.h string.h sys/file.h sys/time.h unistd.h wchar.h wctype.h endian.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_PID_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_FSEEKO
AC_FUNC_MBRTOWC
AC_FUNC_REALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([btowc doprnt floor gettimeofday isascii mbrlen memmove memset nl_langinfo re_comp regcomp strchr strerror strtol strtoumax])

AC_CONFIG_FILES([Makefile
                 SFMT/Makefile])
#AC_CONFIG_SUBDIRS([bignum/gmp
#                   bignum/mpfr
#                   gc/bdwgc
#                   gc/libatomic_ops])
AC_OUTPUT
