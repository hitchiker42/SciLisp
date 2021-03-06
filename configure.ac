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
AC_CHECK_PROG(HAVE_EMACS,emacs,yes,
  [AC_MSG_WARN([Emacs not found in default PATH, \
 you will be unable to modify SciLisp primitives])] no)
# Checks for libraries.
AC_CHECK_LIB([gc], [GC_malloc])
AC_CHECK_LIB([cord], [CORD_cat])
dnl AC_CHECK_LIB([clang], [clang_visitChildren])
AC_CHECK_LIB([dl], [dlsym])
AC_CHECK_LIB([gmp], [__gmpz_add],,
                    [AC_MSG_ERROR([Error, required library gmp not found])])
AC_CHECK_LIB([m], [cos])
AC_CHECK_LIB([mpfr], [mpfr_init2],,
                    [AC_MSG_ERROR([Error, required library mpfr not found])])
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([ffi], [ffi_call])
AC_ARG_WITH([readline],
  [AS_HELP_STRING([--with-readline],
    [Use readline for command line editing (default=check)])],
  [],
  [with_readline=check])
AS_IF([test "x$with_readline" != xno],
  [AC_CHECK_LIB([readline], [readline],
     [AC_DEFINE([HAVE_READLINE], [1],
                [Define if readline is enabled])
                [LIBS="$LIBS -lreadline"]])],
    [if test "x$with_readline" != xcheck; then
        AC_MSG_FAILURE(
          [--with-readline was given, but test for readline failed])
     fi])
AC_ARG_WITH([gc-threads],
  [AS_HELP_STRING([--with-gc-threads],
    [enable multithreaded gc (default=yes)])],
  [],
  [with_gc_threads=yes])
AS_IF([test "x$with_gc_threads" = xyes],
  [AC_DEFINE([GC_THREADS],[1],[Define if using multithreaded gc])])
AC_ARG_ENABLE([threads],
  [AS_HELP_STRING([--enable-threads],
    [enable multithreded scilisp (does not effect gc threads)])],
  [],
  [enable_threads=no])
dnl Shold insure that using threads forces gc threads
AS_IF([test "x$enable_threads" = xyes],
  [AC_DEFINE([GC_THREADS],[1],[Define if using multithreaded gc])]
  [AC_DEFINE([MULTI_THREADED], [1], [Define if using threads])])
AC_ARG_WITH([thread-local-alloc],
  [AS_HELP_STRING([--with-thread-local-alloc],
    [Use thread local allocation, default no.
    Set to force to enable in single threaded programs])],
    [],
    [with_thread_local_alloc=no])
AS_IF([test "x$with_thread_local_alloc" = xforce],
  [AC_DEFINE([GC_THREAD_LOCAL,[1],[Set if using thread local allocation]])],
  [test "x$with_thread_local_alloc" = xyes && "x$enable_threads" = xyes],
  [AC_DEFINE([GC_THREAD_LOCAL,[1],[Set if using thread local allocation]])])
AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
    [enable debugging messages (current default=yes)])],
  [],
  [enable_debug=yes])
AS_IF([test "x$enable_debug" = xyes],
  [AC_DEFINE([DEBUG],[1],[Define if debugging is enabled])],
  [AC_DEFINE([NDEBUG],[1],[Define if debugging is disabled])])
AC_ARG_ENABLE([link-time-optimization],
  [AS_HELP_STRING([--enable-link-time-optimization],
    [Build SciLisp with link time optimization (default, yes if using gcc otherwise no)])],
  [],
  [AS_IF([test "x$GCC" = xyes],
      [enable_link_time_optimization=yes],
      [enable_link_time_optimization=no])])
AS_IF([test "x$enable_link_time_optimization" = xyes],
  [CFLAGS="${CFLAGS} -flto"])
AC_ARG_ENABLE([optimized],
  [AS_HELP_STRING([--enable-optimized],
    [Enable nonportable optimizations (default=no) (default optimization is -O2)])],
  [],
  [enable_optimized=no])
AS_IF([test "x$enable_optimized" = xyes],
  [CFLAGS="${CFLAGS} -O3 -march=native"])
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
AC_FUNC_ALLOCA
AC_CHECK_FUNCS([btowc doprnt floor gettimeofday isascii mbrlen memmove memset nl_langinfo re_comp regcomp strchr strerror strtol strtoumax])

#architecture check (and if cpu == x86_64 check available features)
AC_MSG_CHECKING([if compiling for x86_64])
AC_CANONICAL_HOST
AS_IF([test "x$host_cpu" = xx86_64],
        [AC_MSG_RESULT([yes])]
        [AC_MSG_CHECKING([for latest supported simd instruction set])]
        [AS_IF([test -x extra/cpuid],
               [simd_features=`extra/cpuid -l`],
               [simd_features=sse2])]
        [AC_SUBST(SIMD_SET,$simd_features)]
        [AC_MSG_RESULT([${simd_features}])],
        [AC_MSG_RESULT([no])])
dnl[AS_IF([test -x extra/cpuid],
dnl[AC_SUBST(SIMD_SET,[-msse2])])
AC_CONFIG_FILES([Makefile
                 SFMT/Makefile])
#AC_CONFIG_SUBDIRS([bignum/gmp
#                   bignum/mpfr
#                   gc/bdwgc
#                   gc/libatomic_ops])
AC_OUTPUT
