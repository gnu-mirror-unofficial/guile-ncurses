/*
  panel_type.c

  Copyright 2009, 2010, 2011, 2014, 2016, 2019 Free Software Foundation, Inc.

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

#include "panel_type.h"
#include "type.h"

SCM equalp_panel (SCM x1, SCM x2);
size_t gc_free_panel (SCM x);
SCM mark_panel (SCM x);
int print_panel (SCM x, SCM port, scm_print_state * pstate);

/* panel -- in C, a window foreign object that has a non-NULL panel. In
   Scheme, a foreign object that contains a pointer to that
   structure. */

int
_scm_is_panel (SCM x)
{
  if (_scm_is_window (x))
    {
      if (scm_foreign_object_ref (x, 1) == NULL)
        return 0;
      else
        return 1;
    }
  else
    return 0;
}

PANEL *
_scm_to_panel (SCM x)
{
  assert (_scm_is_window (x));
  PANEL *panel = scm_foreign_object_ref (x, 1);
  assert (panel != NULL);

  return panel;
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
  else if (panel1 != panel2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

/* The curses primitive that frees memory is called del_panel. Note
   that del_panel doesn't free the underlying window. */
size_t
free_panel (SCM x)
{
  scm_assert_foreign_object_type (window_fo_type, x);
  if (_scm_is_panel (x))
    {
      // Window has an associated panel
      PANEL *panel = scm_foreign_object_ref (x, 1);

      if (panel)
        {
          set_panel_userptr (panel, NULL);
          int retval = del_panel (panel);
          if (retval != OK)
            {
              scm_error_scm (scm_from_locale_symbol ("ncurses"),
                             scm_from_locale_string ("freeing panel"),
                             scm_from_locale_string ("bad argument"),
                             SCM_BOOL_F, SCM_BOOL_F);
            }
          scm_foreign_object_set_x (x, 1, NULL);
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
  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "make-panel!");

  if (_scm_is_panel (win))
    scm_misc_error ("make-panel!", "already a panel ~A", scm_list_1 (win));

  WINDOW *c_win = scm_foreign_object_ref (win, 0);
  if (c_win)
    {
      PANEL *panel = new_panel (c_win);

      if (panel == NULL)
        scm_misc_error ("make-panel!", "bad window ~A", scm_list_1 (win));

      /* We need a reference back to the parent #<window> so we can
         write a panel. */
      assert (!SCM_IMP (win));
      set_panel_userptr (panel, SCM2PTR (win));
      scm_foreign_object_set_x (win, 1, panel);
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
