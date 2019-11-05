/*
  menu_type.h

  Copyright 2009, 2010, 2014, 2016, 2019 Free Software Foundation, Inc.

  This file is part of GNU Guile-Ncurses.

  Guile-Ncurses is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-Ncurses is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Guile-Ncurses.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#ifndef MENU_TYPE_H
#define MENU_TYPE_H

#include <libguile.h>
#include "visibility.h"

#if HAVE_CURSES_H
#include <menu.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/menu.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/menu.h>
#else
#error "No menu.h file included"
#endif

GUCU_LOCAL int _scm_is_item (SCM x);
GUCU_LOCAL ITEM *_scm_to_item (SCM x);
GUCU_LOCAL SCM _scm_from_item (ITEM *x);
GUCU_LOCAL void item_init_refcount (ITEM *item);
GUCU_LOCAL bool item_increase_refcount (ITEM *item);
GUCU_LOCAL bool item_decrease_refcount (ITEM *item);
GUCU_LOCAL int item_get_refcount (ITEM *item);

GUCU_API SCM gucu_is_item_p (SCM x);
GUCU_API SCM gucu_item_equal_p (SCM item1, SCM item2);
GUCU_API SCM gucu_new_item (SCM x, SCM y);

GUCU_LOCAL int _scm_is_menu (SCM x);
GUCU_LOCAL MENU *_scm_to_menu (SCM x);
GUCU_LOCAL SCM _scm_from_menu (MENU *x);

GUCU_API SCM gucu_is_menu_p (SCM x);
GUCU_API SCM gucu_menu_equal_p (SCM menu1, SCM menu2);
GUCU_API SCM gucu_new_menu (SCM x);

GUCU_LOCAL void gucu_menu_init_type (void);

#endif
