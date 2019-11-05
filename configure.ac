# Process this file with autoconf to produce a configure script

# Copyright (C) 2009, 2010, 2011, 2013, 2014, 2016, 2017, 2019 Free Software Foundation, Inc.
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
AC_INIT([GNU Guile-Ncurses],[3.0],[bug-guile-ncurses@gnu.org])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])

AC_PREREQ(2.60)
AC_USE_SYSTEM_EXTENSIONS([_GNU_SOURCE])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_MACRO_DIR([m4])

##################
# Set the library libtool version constant
LIBGUILE_NCURSES_INTERFACE="12:0:0"
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
GUILE_PKG([2.2 3.0])
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

AC_MSG_CHECKING([the Guile effective version])
guile_effective_version="`$GUILE -c '(display (effective-version))'`"
AC_MSG_RESULT([$guile_effective_version])

################
# Guile has three directories that don't follow the GNU Filesystem
# Heirarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([gnu-filesystem-hierarchy],
            [AS_HELP_STRING([--with-gnu-filesystem-hierarchy],
                            [Strictly follow GNU Filesystem Heirarchy when installing, instead of querying Guile to discover the install directories that it expects.])])

case "x$with_gnu_filesystem_hierarchy" in
     xyes)
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
     *)
       # Get the installation directories from Guile
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir=$GUILE_SITE
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir=$GUILE_EXTENSION
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir=$GUILE_SITE_CCACHE
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
esac

# The curses man page lists the following possibilities for curses
# headers

AC_ARG_WITH(ncursesw,
            [AS_HELP_STRING([--without-ncursesw],
                            [Don't use ncursesw (wide character support)])],
            [with_ncursesw=no],
            [with_ncursesw=check])

AC_MSG_NOTICE([checking for ncurses])

have_ncursesw_curses_h=no

if test "$with_ncursesw" = check
then
  AC_CHECK_HEADER([ncursesw/curses.h],
                  [AC_DEFINE([HAVE_NCURSESW_CURSES_H],
                             [1],
                             [Define if you have ncursesw/curses.h])
                   have_ncursesw_curses_h=yes])
fi

if test "$have_ncursesw_curses_h" = no
then
  AC_CHECK_HEADER([ncurses/curses.h],
                  [AC_DEFINE([HAVE_NCURSES_CURSES_H],
                             [1],
                             [Define if you have ncurses/curses.h])
                 have_ncurses_curses_h=yes],
                [have_ncurses_curses_h=no])
fi

if test "$have_ncurses_curses_h" = no && test "$have_ncursesw_curses_h" = no
then
  AC_CHECK_HEADER([curses.h],
                  [AC_DEFINE([HAVE_CURSES_H],
                             [1],
                             [Define if you have curses.h])],
                  [AC_MSG_FAILURE([curses.h could not be found])])
fi

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

AC_MSG_NOTICE([checking for term.h])

have_ncursesw_term_h=no

if test "$with_ncursesw" = yes
then
  AC_CHECK_HEADER([ncursesw/term.h],
                  [AC_DEFINE([HAVE_NCURSESW_TERM_H],
                             [1],
                             [Define if you have ncursesw/term.h])
                   have_ncursesw_term_h=yes])
fi

if test "$have_ncursesw_term_h" = no
then
  AC_CHECK_HEADER([ncurses/term.h],
                  [AC_DEFINE([HAVE_NCURSES_TERM_H],
                             [1],
                             [Define if you have ncurses/term.h])
                   have_ncurses_term_h=yes],
                  [have_ncurses_term_h=no])
fi

if test "$have_ncurses_term_h" = no && test "$have_ncursesw_term_h" = no
then
  AC_CHECK_HEADER([term.h],
                  [AC_DEFINE([HAVE_TERM_H],
                             [1],
                             [Define if you have term.h])])
fi


AC_CHECK_HEADER([termios.h],
                [AC_DEFINE([HAVE_TERMIOS_H],
                           [1],
                           [Define if you have termios.h])
                 have_termios_h=yes],
                [have_termios_h=no])


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
if test "$have_ncursesw_curses_h" = yes
then
  AC_CHECK_SIZEOF([chtype], ,[#include <ncursesw/curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <ncursesw/curses.h>])
  AX_CHECK_DEFINE([ncursesw/curses.h],[BUTTON5_PRESSED],[HAVE_BUTTON5=1],[HAVE_BUTTON5=0])
  AX_CHECK_DEFINE([ncursesw/curses.h],[KEY_EVENT],[HAVE_KEY_EVENT=1],[HAVE_KEY_EVENT=0])
  AX_CHECK_DEFINE([ncursesw/menu.h], [O_MOUSE_MENU],[HAVE_O_MOUSE_MENU=1],[HAVE_O_MOUSE_MENU=0])
else
if test "$have_ncurses_curses_h" = yes
then
  AC_CHECK_SIZEOF([chtype], ,[#include <ncurses/curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <ncurses/curses.h>])
  AX_CHECK_DEFINE([ncurses/curses.h],[BUTTON5_PRESSED],[HAVE_BUTTON5=1],[HAVE_BUTTON5=0])
  AX_CHECK_DEFINE([ncurses/curses.h],[KEY_EVENT],[HAVE_KEY_EVENT=1],[HAVE_KEY_EVENT=0])
  AX_CHECK_DEFINE([ncurses/menu.h], [O_MOUSE_MENU],[HAVE_O_MOUSE_MENU=1],[HAVE_O_MOUSE_MENU=0])
else
  AC_CHECK_SIZEOF([chtype], ,[#include <curses.h>])
  AC_CHECK_SIZEOF([attr_t], ,[#include <curses.h>])
  AX_CHECK_DEFINE([curses.h],[BUTTON5_PRESSED],[HAVE_BUTTON5=1],[HAVE_BUTTON5=0])
  AX_CHECK_DEFINE([curses.h],[KEY_EVENT],[HAVE_KEY_EVENT=1],[HAVE_KEY_EVENT=0])
  AX_CHECK_DEFINE([menu.h], [O_MOUSE_MENU],[HAVE_O_MOUSE_MENU=1],[HAVE_O_MOUSE_MENU=0])
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

m4_define([hardening_flags],[ dnl
 -D_FORTIFY_SOURCE=2 dnl
 -DSCM_DEBUG_TYPING_STRICTNESS=2 dnl
 -O2 dnl
 -pipe dnl
 -g dnl
 -grecord-gcc-switches dnl
 -Wall dnl
 -Werror=format-security dnl
 -Werror=implicit-function-declaration dnl
 -Werror=shadow dnl
 -fno-omit-frame-pointer dnl
 -fasynchronous-unwind-tables dnl
 -fexceptions dnl
 -fvar-tracking dnl
 -fstack-protector-strong dnl
 -fstack-clash-protection dnl
 -fcf-protection dnl
 -fplugin=annobin dnl
 -Wl,--dynamic-list-data dnl
 -Wl,-z,defs dnl
 -Wl,-z,now dnl
 -Wl,-z,relro dnl
 -Wsuggest-attribute=format dnl
 -Wsuggest-attribute=malloc dnl
 -Wsuggest-attribute=noreturn dnl
 -Wsuggest-attribute=pure dnl
 -fdiagnostics-color=auto dnl
])

AC_ARG_ENABLE([hardening],
              [AS_HELP_STRING([--enable-hardening],
                              [compile with common hardening flags])])
AS_IF([test "x$enable_hardening" = xyes],
      [CC_CHECK_CFLAGS_APPEND([hardening_flags])])

##################
# Check for C library functions
AC_CHECK_FUNCS([cfmakeraw fopencookie ptsname posix_openpt grantpt unlockpt tcgetsid wcwidth])

# Check for ncurses library functions
AC_CHECK_FUNCS([key_defined term_attrs set_tabsize is_pad is_subwin form_driver_w setupterm wgetdelay has_mouse])

# Later something should be done with this
# AC_FUNC_MALLOC

AC_ARG_ENABLE([termios],
              [AS_HELP_STRING([--disable-termios],
                              [disable support for termios functions])],
            [],
            [enable_termios=yes])
AM_CONDITIONAL([TERMIOS], [test "$enable_termios" = yes])

AS_IF([test "x$enable_termios" != xno && test "x$have_termios_h" = xyes ],
      [AC_DEFINE([ENABLE_TERMIOS], [1],
                 [Define to enable TERMIOS support])

       ## DEFAULT CONSTANTS
       # assume OPOST, TCIFLUSH, TCIOFF, TCIOFLUSH, TCION, TCOFLUSH,
       # TCOOFF, TCOON, TCSADRAIN, TCSAFLUSH, TCSANOW are always
       # present

       # Check for some of the XOpen and old BSD termios constants.
       # I'm intentionally ignoring all of the delays like NL0 and VT0.
       AC_CHECK_DECLS([OCRNL, OFILL, ONLCR, ONOCR, ONLRET], [], [],
       [[
       #include <termios.h>
       ]])

       ## SPECIAL CONTROL CHARACTERS
       # assume VEOF, VEOL, VERASE, VINTR, VKILL, VMIN, VQUIT, VSTART,
       # VSTOP, VSUSP always present

       ## INPUT FLAGS
       # assume BRKINT, ICRNL, IGNBRK, IGNCR, IGNPAR, INLCR, INPCK,
       # ISTRIP, IXOFF, IXON, PARMRK are always present

AC_CHECK_DECLS([IXANY], [], [],
       [[
       #include <termios.h>
       ]])

       ## OUTPUT FLAGS

       ## CONTROL FLAGS
       # assume CLOCAL, CREAD, CS5, CS6, CS7, CS8, CSIZE, CSTOPB,
       # HUPCL, PARENB, PARODD, VTIME are always present

       ## LOCAL FLAGS
       # assume ECHO, ECHOE, ECHOK, ECHONL, ICANON, IEXTEN, ISIG,
       # NOFLSH, TOSTOP are always present

       ## Standard speeds
       # assume B0, B110, B1200, B134, B150, B1800, B19200, B200, B2400,
       # B300, B38400, B4800, B50, B600, B75, B9600 are always present

       # Check for rarer termios baud rate constants
       AC_CHECK_DECLS([B7200, B14400, B28800, B57600, B76800, B115200, B230400], [], [],
       [[
       #include <termios.h>
       ]])])

AC_CHECK_DECLS([O_RDWR, O_NOCTTY], [], [],
                [[
                  #include <stdlib.h>
                  #include <fcntl.h>
                ]])

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

if test "$with_gnu_filesystem_hierarchy" = "yes"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   [This is because you ran configure with the `--with-gnu-filesystem-hierarchy' flag]
   []
   [Remember that you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
else
   AC_MSG_NOTICE([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   [These directories are the ones expected by $GUILE])
fi
AC_SUBST([guilesitedir])
