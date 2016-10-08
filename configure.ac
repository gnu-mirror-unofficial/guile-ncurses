# Process this file with autoconf to produce a configure script

# Copyright (C) 2009, 2010, 2011, 2013, 2014, 2016 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.


##################
# Package info
AC_INIT([GNU Guile-Ncurses],[1.7],[bug-guile-ncurses@gnu.org])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])

AC_PREREQ(2.60)
AC_USE_SYSTEM_EXTENSIONS([_GNU_SOURCE])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_MACRO_DIR([m4])

##################
# Set the library libtool version constant
LIBGUILE_NCURSES_INTERFACE="10:0:0"
AC_SUBST(LIBGUILE_NCURSES_INTERFACE)

##################
# Check for Programs
PKG_PROG_PKG_CONFIG
AC_PROG_CC
AC_CANONICAL_HOST
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AM_PROG_AR

# from guile.m4
# sets: GUILE_CFLAGS and GUILE_LDFLAGS
GUILE_PROGS
GUILE_FLAGS

AC_PATH_PROG([GUILD],[guild])
AC_SUBST([GUILD])

AC_MSG_CHECKING([the Guile effective version])
guile_effective_version="`$GUILE -c '(display (effective-version))'`"
AC_MSG_RESULT([$guile_effective_version])

################
# Guile has three directories that don't follow the GNU Filesystem
# Heirarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([guile-install-directories],
            [AS_HELP_STRING([--with-guile-install-directories],
                            [Don't follow GNU Filesystem Heirarchy when installing.  Instead install to the directories that Guile expects.])])

case "x$with_guile_install_directories" in
     xyes)
       # Get the installation directories from Guile
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir="`$PKG_CONFIG guile-$guile_effective_version --variable=sitedir`"
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir="`$PKG_CONFIG guile-$guile_effective_version --variable=extensiondir`"
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir="`$GUILE -c '(display (cadr %load-compiled-path))'`"
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
     *)
       # Follow the GNU Filesystem Heirarchy Standard correctly
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir="\$(datadir)/guile/site/$guile_effective_version"
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir="$libdir/guile/$guile_effective_version"
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir="$libdir/guile/$guile_effective_version/site-ccache"
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
esac

# The curses man page lists the following possibilities for curses
# headers

AC_MSG_NOTICE([checking for ncurses])

AC_CHECK_HEADER([curses.h],
                [AC_DEFINE([HAVE_CURSES_H],
                           [1],
                           [Define if you have curses.h])
                 have_curses_h=yes],
                [have_curses_h=no])

if test "$have_curses_h" = no
then
  AC_CHECK_HEADER([ncurses/curses.h],
                  [AC_DEFINE([HAVE_NCURSES_CURSES_H],
                             [1],
                             [Define if you have ncurses/curses.h])
                 have_ncurses_curses_h=yes],
                [have_ncurses_curses_h=no])
fi

if test "$have_curses_h" = no && test "$have_ncurses_curses_h" = no
then
  AC_CHECK_HEADER([ncursesw/curses.h],
                  [AC_DEFINE([HAVE_NCURSESW_CURSES_H],
                             [1],
                             [Define if you have ncursesw/curses.h])],
                  [AC_MSG_FAILURE([curses.h could not be found])])
fi

AC_ARG_WITH(ncursesw,
            [AS_HELP_STRING([--without-ncursesw],
                            [Don't use ncursesw (wide character support)])],
            [with_ncursesw=no],
            [with_ncursesw=check])

AS_IF([test "$with_ncursesw" = check],
      [AC_SEARCH_LIBS([initscr], [ncursesw],
                      [AC_DEFINE([HAVE_NCURSESW],
                                 [1],
                                 [Define if you have libncursesw])
                       with_ncursesw=yes],
                      [AC_MSG_FAILURE([--with-ncursesw was given, but, test for ncursesw failed])])])

AS_IF([test "$with_ncursesw" = no],
      [AC_SEARCH_LIBS([initscr], [ncurses],
                      [AC_DEFINE([HAVE_NCURSES],
                                 [1],
                                 [Define if you have libncurses])],
                      [AC_MSG_FAILURE([the ncurses library was not found])])])

AM_CONDITIONAL([WIDE], [test "$with_ncursesw" = yes])


# Check for the menu, panel, and form libraries
if test "$with_ncursesw" = no
then
  AC_SEARCH_LIBS([new_menu], [menu],
                 [],
                 [AC_MSG_ERROR([the ncurses menu library is required])])
  AC_SEARCH_LIBS([new_form], [form],
                 [],
                 [AC_MSG_ERROR([the ncurses form library is required])])
  AC_SEARCH_LIBS([new_panel], [panel],
                 [],
                 [AC_MSG_ERROR([the ncurses panel library is required])])
else
  AC_SEARCH_LIBS([u32_strconv_from_locale], [unistring],
                 [],
                 [AC_MSG_ERROR([the unistring library is required when wide curses is used])])
  AC_SEARCH_LIBS([new_menu], [menuw],
                 [],
                 [AC_MSG_ERROR([the ncurses menuw library is required])])
  AC_SEARCH_LIBS([new_form], [formw],
                 [],
                 [AC_MSG_ERROR([the ncurses formw library is required])])
  AC_SEARCH_LIBS([new_panel], [panelw],
                 [],
                 [AC_MSG_ERROR([the ncurses panelw library is required])])
fi

##################
# Check for Libtool

# New method for when Libtool 2.x is default
LT_INIT([dlopen disable-fast-install disable-static win32-dll])

##################
# Initial guess of platform specific options

case $host_os in
mingw* | cygwin* | os2* | pw32* | cegcc*)
	[building_dll=yes] ;;
*)
	[building_dll=no] ;;
esac
case $host_os in
mingw* | os2* | pw32* | cegcc*)
	[building_shell=no] ;;
*)
	[building_shell=yes] ;;
esac
case $host_os in
mingw* )
	[dll_version_info=no] ;;
*)
	[dll_version_info=yes] ;;
esac

AM_CONDITIONAL([BUILDING_DLL], [test "$building_dll" = yes])
AM_CONDITIONAL([BUILDING_SHELL], [test "$building_shell" = yes])
AM_CONDITIONAL([DLL_VERSION_INFO], [test "$dll_version_info" = yes])
##################
# Check for libraries

AC_MSG_CHECKING([if guile can handle UCS4 characters])
GUILE_CHECK([ac_cv_guile_ucs4],
            [(= 32764 (false-if-exception (char->integer (integer->char 32764))))])
if test "$ac_cv_guile_ucs4" = 0
then
  ac_cv_guile_ucs4=yes
else
  ac_cv_guile_ucs4=no
fi
AC_MSG_RESULT([$ac_cv_guile_ucs4])
AM_CONDITIONAL([UCS4],[test "$ac_cv_guile_ucs4" = yes])

if test "$ac_cv_guile_ucs4" = yes
then
  AC_CHECK_HEADER([uniconv.h],
                  [AC_DEFINE([HAVE_UNICONV_H],
                             [1],
                             [Define if you have uniconv.h])],
                  [AC_MSG_FAILURE([uniconv.h could not be found])])
fi


AC_ARG_ENABLE([warnings],
              [AS_HELP_STRING([--enable-warnings],
                              [Build with most GCC warnings enabled])])
AM_CONDITIONAL([WARN], [test "$enable_warnings" = yes])

AC_ARG_ENABLE([demo-install],
              [AS_HELP_STRING([--enable-demo-install],
                              [Install the demonstration programs])])
AM_CONDITIONAL([DEMO_INSTALL], [test "$enable_demo_install" = yes])


##################
# Check for header files
AC_CHECK_HEADERS([process.h pty.h util.h])

##################
# Check for types, structures, compiler characteristics
AX_C___ATTRIBUTE__
AC_CHECK_TYPE([off64_t],
              [AC_DEFINE([HAVE_OFF64_T],
                         [1],
                         [Define if you are have the off64_t type])],
              ,
              [#define _LARGEFILE64_SOURCE
               #include <unistd.h>
               ])
AC_CHECK_TYPE([cookie_io_functions_t],
              [AC_DEFINE([HAVE_COOKIE_IO_FUNCTIONS_T],
                         [1],
                         [Define if you are have the cookie_io_functions_t type])],
              ,
              [#include <stdio.h>
               ])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([void *])
if test "$have_curses_h" = yes
then
  AC_CHECK_SIZEOF([chtype], ,[#include <curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <curses.h>])
  AX_CHECK_DEFINE([curses.h],[BUTTON5_PRESSED],[HAVE_BUTTON5=1],[HAVE_BUTTON5=0])
  AX_CHECK_DEFINE([curses.h],[KEY_EVENT],[HAVE_KEY_EVENT=1],[HAVE_KEY_EVENT=0])
  AX_CHECK_DEFINE([menu.h], [O_MOUSE_MENU],[HAVE_O_MOUSE_MENU=1],[HAVE_O_MOUSE_MENU=0])
else
if test "$have_ncurses_curses_h" = yes
then
  AC_CHECK_SIZEOF([chtype], ,[#include <ncurses/curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <ncurses/curses.h>])
  AX_CHECK_DEFINE([ncurses/curses.h],[BUTTON5_PRESSED],[HAVE_BUTTON5=1],[HAVE_BUTTON5=0])
  AX_CHECK_DEFINE([ncurses/curses.h],[KEY_EVENT],[HAVE_KEY_EVENT=1],[HAVE_KEY_EVENT=0])
  AX_CHECK_DEFINE([ncurses/menu.h], [O_MOUSE_MENU],[HAVE_O_MOUSE_MENU=1],[HAVE_O_MOUSE_MENU=0])
else
  AC_CHECK_SIZEOF([chtype], ,[#include <ncursesw/curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <ncursesw/curses.h>])
  AX_CHECK_DEFINE([ncursesw/curses.h],[BUTTON5_PRESSED],[HAVE_BUTTON5=1],[HAVE_BUTTON5=0])
  AX_CHECK_DEFINE([ncursesw/curses.h],[KEY_EVENT],[HAVE_KEY_EVENT=1],[HAVE_KEY_EVENT=0])
  AX_CHECK_DEFINE([ncursesw/menu.h], [O_MOUSE_MENU],[HAVE_O_MOUSE_MENU=1],[HAVE_O_MOUSE_MENU=0])
fi
fi
AS_IF([test "x$HAVE_BUTTON5" = "x1"], AC_DEFINE([HAVE_BUTTON5], 1, [Have BUTTON5 defines]))
AM_CONDITIONAL(HAVE_BUTTON5, test "x$HAVE_BUTTON5" = "x1")
AS_IF([test "x$HAVE_KEY_EVENT" = "x1"], AC_DEFINE([HAVE_KEY_EVENT], 1, [Have KEY_EVENT defines]))
AM_CONDITIONAL(HAVE_KEY_EVENT, test "x$HAVE_KEY_EVENT" = "x1")
AS_IF([test "x$HAVE_O_MOUSE_MENU" = "x1"], AC_DEFINE([HAVE_O_MOUSE_MENU], 1, [Have O_MOUSE_MENU define]))
AM_CONDITIONAL(HAVE_O_MOUSE_MENU, test "x$HAVE_O_MOUSE_MENU" = "x1")


AC_CHECK_SIZEOF([wchar_t], ,[#include <stddef.h>])
AC_CHECK_SIZEOF([wint_t], ,[#include <wchar.h>])

##################
# Check for C library functions
AC_CHECK_FUNCS([cfmakeraw fopencookie ptsname grantpt unlockpt tcgetsid cfsetspeed wcwidth])

# Check for ncurses library functions
AC_CHECK_FUNCS([key_defined term_attrs set_tabsize is_pad is_subwin form_driver_w])

# Later something should be done with this
# AC_FUNC_MALLOC

AC_ARG_ENABLE([termios],
              [AS_HELP_STRING([--disable-termios],
                              [disable support for termios functions])],
            [],
            [enable_termios=yes])
AM_CONDITIONAL([TERMIOS], [test "$enable_termios" = yes])

AS_IF([test "x$enable_termios" != xno],
      [AC_DEFINE([ENABLE_TERMIOS], [1],
                 [Define to enable TERMIOS support])
       # Check for the XOpen and BSD termios constants.
       # Hopefully just one will suffice for the entire set.
       AC_CHECK_DECLS([BS0, ECHOCTL], [], [],
       [[
       #include <termios.h>
       ]])

       # Check for rarer termios constants
       AC_CHECK_DECLS([XCASE, ECHOPRT, PENDIN], [], [],
       [[
       #include <termios.h>
       ]])])

##################
# Check for system services

AC_CHECK_PROG([TOE],[toe],[yes],[no])
AS_IF([test "$TOE" = yes],
      [AC_MSG_CHECKING([for the terminfo of xterm])
       havexterm=no
       if toe -a | grep -q xterm
       then havexterm=yes
       fi
       AC_MSG_RESULT([$havexterm])])
AM_CONDITIONAL([HAVE_XTERM_TERMINFO],[test "$havexterm" = yes])

##################
#  Output

AC_CONFIG_FILES([Makefile src/ncurses/Makefile doc/Makefile m4/Makefile])
AC_CONFIG_FILES([build_aux/Makefile tools/Makefile])
AC_CONFIG_FILES([test/Makefile])
AC_CONFIG_FILES([tools/uninstalled-env], [chmod +x tools/uninstalled-env])
AC_OUTPUT

if test "$with_guile_install_directories" != "yes"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   [You should probably re-run `configure' with]
   [`--with-guile-install-directories]
   [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi
AC_SUBST([guilesitedir])
