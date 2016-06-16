/*
  panel_type.c

  Copyright 2009, 2010, 2011, 2014, 2016 Free Software Foundation, Inc.

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
#include <config.h>

#include <assert.h>
#include <libguile.h>
#include <stdio.h>
#include <string.h>

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

#include "compat.h"
#include "panel_type.h"
#include "type.h"

SCM equalp_panel (SCM x1, SCM x2);
size_t gc_free_panel (SCM x);
SCM mark_panel (SCM x);
int print_panel (SCM x, SCM port, scm_print_state * pstate);


/* panel -- in C, a gucu_window struct that has a non-NULL panel. In
   Scheme, a smob that contains a pointer to that structure. */

int
_scm_is_panel (SCM x)
{
  if (_scm_is_window (x))
    {
      if (SCM_SMOB_DATA (x) == 0)
	return 0;
      else
	{
	  struct gucu_window *wp = (struct gucu_window *) SCM_SMOB_DATA (x);
	  if (wp != NULL && wp->panel != NULL)
	    return 1;
	  else
	    return 0;
	}
    }
  else
    return 0;
}

PANEL *
_scm_to_panel (SCM x)
{
  struct gucu_window *wp;

  assert (_scm_is_window (x));
  wp = (struct gucu_window *) SCM_SMOB_DATA (x);
  assert (wp->panel != NULL);

  return wp->panel;
}

// Panels are equal if they point to the same C structure
SCM
equalp_panel (SCM x1, SCM x2)
{
  if (!_scm_is_panel (x1) || !_scm_is_panel (x2))
    return SCM_BOOL_F;

  PANEL *panel1 = (PANEL *) _scm_to_panel (x1);
  PANEL *panel2 = (PANEL *) _scm_to_panel (x2);

  if ((panel1 == NULL) || (panel2 == NULL))
    return SCM_BOOL_F;
  else if ((panel1 != panel2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

/* The curses primitive that frees memory is called del_panel. Note
   that del_panel doesn't free the underlying window. */
size_t
free_panel (SCM x)
{
  struct gucu_window *wp;
  int retval;

  scm_assert_smob_type (window_tag, x);
  if (_scm_is_panel (x))
    {
      // Window has an associated panel
      wp = (struct gucu_window *) SCM_SMOB_DATA (x);
      if (wp && wp->window && wp->panel)
	{
	  int retval = del_panel (wp->panel);
	  if (retval != OK)
	    {
	      scm_error_scm (scm_from_locale_symbol ("ncurses"),
			     scm_from_locale_string ("freeing panel"),
			     scm_from_locale_string ("bad argument"),
			     SCM_BOOL_F, SCM_BOOL_F);
	    }
	  wp->panel = (PANEL *) NULL;
	}
    }

  return 0;
}

SCM
gucu_del_panel_x (SCM x)
{
  SCM_ASSERT (_scm_is_window (x), x, SCM_ARG1, "del-panel");
  free_panel (x);

  return SCM_UNSPECIFIED;
}

SCM
gucu_is_panel_p (SCM x)
{
  return scm_from_bool (_scm_is_panel (x));
}


SCM
gucu_make_panel_x (SCM win)
{
  struct gucu_window *wp = NULL;
  SCM smob;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "make-panel!");

  if (_scm_is_panel (win))
    scm_misc_error ("make-panel!", "already a panel ~A", scm_list_1(win));

  wp = (struct gucu_window *) SCM_SMOB_DATA(win);
  if (wp && wp->window)
    {
      wp->panel = new_panel (wp->window);
      if (wp->panel == NULL)
	scm_misc_error ("make-panel!", "bad window ~A", scm_list_1 (win));
    }
  return SCM_UNSPECIFIED;
}


void
gucu_panel_init_type ()
{
  scm_c_define_gsubr ("panel?", 1, 0, 0, gucu_is_panel_p);
  scm_c_define_gsubr ("make-panel!", 1, 0, 0, gucu_make_panel_x);
  scm_c_define_gsubr ("del-panel!", 1, 0, 0, gucu_del_panel_x);
  scm_c_define_gsubr ("panel=?", 2, 0, 0, equalp_panel);
}
