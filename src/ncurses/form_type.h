/*
  form_type.h

  Copyright 2009, 2010, 2014, 2016, 2019 Free Software Foundation, Inc.

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
#ifndef FORM_TYPE_H
#define FORM_TYPE_H

#include <libguile.h>
#include "visibility.h"

#if HAVE_CURSES_H
#include <form.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/form.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/form.h>
#else
#error "No curses.h file included"
#endif

GUCU_LOCAL int _scm_is_field (SCM x);
GUCU_LOCAL FIELD *_scm_to_field (SCM x);
GUCU_LOCAL SCM _scm_from_field (FIELD *x);
GUCU_LOCAL void field_init_refcount (FIELD *item);
GUCU_LOCAL bool field_increase_refcount (FIELD *item);
GUCU_LOCAL bool field_decrease_refcount (FIELD *item);
GUCU_LOCAL int field_get_refcount (FIELD *item);
GUCU_LOCAL int _scm_is_list_of_fields (SCM fields);
GUCU_LOCAL int _scm_is_list_of_unattached_fields (SCM fields);
GUCU_LOCAL void gc_free_form (SCM field);
GUCU_LOCAL void gc_free_field (SCM field);

GUCU_LOCAL int _scm_is_form (SCM x);
GUCU_LOCAL FORM *_scm_to_form (SCM x);

GUCU_API SCM gucu_new_field (SCM height, SCM width, SCM top, SCM left,
                             SCM offscreen, SCM nbuffers);
GUCU_API SCM gucu_is_field_p (SCM x);
GUCU_API SCM gucu_field_refcount (SCM x);

GUCU_API SCM gucu_is_form_p (SCM x);
GUCU_API SCM gucu_new_form (SCM fields);
GUCU_API SCM gucu_form_fields (SCM form);
GUCU_API SCM gucu_set_form_fields_x (SCM form, SCM fields);

GUCU_LOCAL void gucu_form_init_type (void);

#endif
