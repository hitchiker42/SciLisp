#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([SciLisp], [0.01])
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([debug.h])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC_C99[(gcc cc clang)]
AC_PROG_LEX

# Checks for libraries.
AC_CHECK_LIB([cord], [CORD_cat])
AC_CHECK_LIB([gc], [GC_malloc])
AC_CHECK_LIB([m], [cos])
AC_CHECK_LIB([readline], [readline])

# Checks for header files.
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([fcntl.h float.h inttypes.h langinfo.h libintl.h limits.h locale.h malloc.h stddef.h stdlib.h string.h sys/file.h unistd.h wchar.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FSEEKO
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([doprnt memset nl_langinfo strchr strerror strtol strtoumax])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
