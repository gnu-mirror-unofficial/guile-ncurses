/*
  panel_type.h

  Copyright 2009, 2010, 2014, 2016 Free Software Foundation, Inc.

  This file is part of GNU Guile-Ncurses.

  Guile-Ncurses is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-Ncurses is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Guile-Ncurses.  If not, see
  <http://www.gnu.org/licenses/>.
*/
#ifndef PANEL_TYPE_H
#define PANEL_TYPE_H

#include <libguile.h>
#include "visibility.h"

#if HAVE_CURSES_H
#include <curses.h>
#include <panel.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#include <ncurses/panel.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/curses.h>
#include <ncursesw/panel.h>
#else
#error "No panel.h file included"
#endif


GUCU_LOCAL int _scm_is_panel (SCM x);
GUCU_LOCAL PANEL *_scm_to_panel (SCM x);

GUCU_API SCM equalp_panel (SCM a, SCM b);
GUCU_API SCM gucu_is_panel_p (SCM x);
GUCU_API SCM gucu_make_panel_x (SCM arg1);
GUCU_API SCM gucu_del_panel_x (SCM arg1);

GUCU_API void gucu_panel_init_type (void);

#endif
