/*
  extra_type.c

  Copyright 2009, 2010, 2011, 2016, 2019 Free Software Foundation, Inc.

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
#ifdef ENABLE_TERMIOS
#include <termios.h>
#endif

#include "extra_type.h"

#ifdef ENABLE_TERMIOS

SCM termios_fo_type;

void gc_free_termios (SCM x);

/* termios -- in C, a TERMIOS.  In Scheme, a foreign object that
 * contains the pointer */

int
_scm_is_termios (SCM x)
{
  if (SCM_IS_A_P (x, termios_fo_type))
    {
      if (scm_foreign_object_ref (x, 0) == NULL)
        return 0;
      else
        return 1;
    }
  else
    return 0;
}

struct termios *
_scm_to_termios (SCM x)
{
  struct termios *gp;

  scm_assert_foreign_object_type (termios_fo_type, x);

  gp = (struct termios *) scm_foreign_object_ref (x, 0);

  return gp;
}

SCM
_scm_from_termios (struct termios *x)
{
  SCM s_termios;

  assert (x != NULL);

  s_termios = scm_make_foreign_object_1 (termios_fo_type, x);

  assert (x == (struct termios *) scm_foreign_object_ref (s_termios, 0));

#if 0
  if (0)
    {
      fprintf (stderr, "Making foreign object from termios based on *%p\n",
               x);
    }
#endif

  return (s_termios);
}

void
gc_free_termios (SCM x)
{
  struct termios *gp;

  gp = (struct termios *) scm_foreign_object_ref (x, 0);

  assert (gp != NULL);
  if (0)
    {
      fprintf (stderr, "Freeing termios at %p\n", gp);
      fprintf (stderr, "Flags: I %u O %u C %u L %u\n", gp->c_iflag,
               gp->c_oflag, gp->c_cflag, gp->c_lflag);
      fprintf (stderr, "Speed: O %u I %u\n", cfgetospeed (gp),
               cfgetispeed (gp));
      fflush (stderr);
      sleep (1);
    }

  scm_gc_free (gp, sizeof (struct termios), "termios");

  scm_foreign_object_set_x (x, 0, NULL);
}

SCM
gucu_is_termios_p (SCM x)
{
  return scm_from_bool (_scm_is_termios (x));
}

SCM
gucu_new_termios (void)
{
  struct termios *gp;
  SCM fo;

  /* Step 1: Allocate memory */
  gp = scm_gc_malloc (sizeof (struct termios), "termios");

  /* Step 2: initialize it with C code */
  memset (gp, 0, sizeof (struct termios));
  gp->c_cflag = CS8;

  /* Step 3: create the foreign object */
  fo = scm_make_foreign_object_1 (termios_fo_type, gp);

  /* Step 4: finish the initialization */
  return fo;
}
#endif /* ENABLE_TERMIOS */

void
gucu_extra_init_type ()
{
#ifdef ENABLE_TERMIOS
  termios_fo_type =
    scm_make_foreign_object_type (scm_from_utf8_symbol ("termios"),
                                  scm_list_1 (scm_from_utf8_symbol ("data")),
                                  gc_free_termios);
  scm_c_define_gsubr ("termios?", 1, 0, 0, gucu_is_termios_p);

  scm_c_define_gsubr ("new-termios", 0, 0, 0, gucu_new_termios);
#endif /* ENABLE_TERMIOS */
}
