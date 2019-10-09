## Process this file with automake to produce Makefile.in

# Makefile.am for GNU Guile-Ncurses.
#
# Copyright 2009, 2010, 2014 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

guileextension_LTLIBRARIES = libguile-ncurses.la
libguile_ncurses_la_SOURCES = curs_const.c		\
	curs_const.h curs_func.c curs_func.h curs_port.c curs_port.h	\
	curs_spec.c curs_spec.h curs_main.c eti_const.c eti_const.h	\
	eti_main.c form_const.c form_const.h form_func.c form_func.h	\
	form_spec.c form_spec.h form_type.c form_type.h form_main.c	\
	extra_const.h extra_const.c extra_func.h extra_func.c           \
	extra_main.c extra_type.h extra_type.c                          \
	gucuconfig.h menu_const.c menu_const.h menu_func.c menu_func.h	\
	menu_main.c menu_spec.c menu_spec.h menu_type.c menu_type.h	\
	panel_func.c panel_func.h panel_main.c panel_spec.c		\
	panel_spec.h panel_type.c panel_type.h slk_func.c slk_func.h	\
	type.c type.h                                                   \
	unicode.c unicode.h visibility.h

libguile_ncurses_la_CPPFLAGS = $(GUILE_CFLAGS)

if WARN
libguile_ncurses_la_CPPFLAGS += -Wall -Wextra -Wstrict-prototypes	\
-Wmissing-prototypes -Wpointer-arith -Wreturn-type -Wcast-qual		\
-Wswitch -Wshadow -Wcast-align -Wwrite-strings -Wchar-subscripts	\
-Winline -Wnested-externs -Wredundant-decls -Wunused -Wformat=2
endif

if WIDE
libguile_ncurses_la_CPPFLAGS += -D_XOPEN_SOURCE_EXTENDED
endif

if BUILDING_DLL
libguile_ncurses_la_CPPFLAGS += -DGUCU_DLL -DGUCU_DLL_EXPORTS
endif

libguile_ncurses_la_LDFLAGS = -no-undefined -version-info	\
$(LIBGUILE_NCURSES_INTERFACE) $(GUILE_LDFLAGS)

if !DLL_VERSION_INFO
libguile_ncurses_la_LDFLAGS += -avoid-version
endif

### scheme files ###
pkgguilesitedir = $(guilesitedir)/ncurses
dist_pkgguilesite_DATA = curses.scm menu.scm panel.scm form.scm	\
	eti.scm slk.scm extra.scm

### compiled scheme files ###

pkgguileobjectdir = $(guileobjectdir)/ncurses
pkgguileobject_DATA = $(dist_pkgguilesite_DATA:%.scm=%.go)

$(pkgguileobject_DATA): libguile-ncurses.la
extra.go: curses.go
form.go: eti.go
menu.go: eti.go


GUILEC_FLAGS = -Warity-mismatch -Wformat -Wunused-variable

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILD) compile --target="$(host)" $(GUILEC_FLAGS) \
	 -L "$(abs_builddir)/.." -L "$(abs_srcdir)/.." -o "$@" "$<"

ETAGS_ARGS = curses.scm menu.scm panel.scm form.scm eti.scm slk.scm	\
	extra.scm

CLEANFILES = $(pkgguileobject_DATA)
