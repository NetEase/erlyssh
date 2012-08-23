#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([readline_drv], [0.1], [radiumce@gmail.com])
AM_INIT_AUTOMAKE([foreign])
AC_PROG_LIBTOOL
AC_CONFIG_SRCDIR([native/linux/readline_drv.c])

AC_ERLANG_NEED_ERLC
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
# Checks for programs.
AC_PROG_CC

CFLAGS="$CFLAGS -L/opt/local/lib -L/usr/lib -L/usr/local/lib -I/opt/local/include/readline -I/opt/include -I/usr/local/include"


# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([getcwd isascii select strdup strerror strrchr])

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
