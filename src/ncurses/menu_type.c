/*
  menu_type.c

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
#include <errno.h>
#include <libguile.h>
#include <stdio.h>
#include <string.h>

#if HAVE_CURSES_H
#include <menu.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/menu.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/menu.h>
#else
#error "No menu.h file included"
#endif

#include "menu_type.h"
#include "type.h"

SCM menu_fo_type;
SCM item_fo_type;

void gc_free_item (SCM x);
void gc_free_menu (SCM x);

/* item -- in C, an ITEM *.  In Scheme, a foreign object that contains the
 * pointer */

void
item_init_refcount (ITEM *item)
{
  set_item_userptr (item, (void *) 1);
}

bool
item_increase_refcount (ITEM *item)
{
  void *ptr = item_userptr (item);
  if (ptr >= (void *) INT_MAX)
    return FALSE;

  set_item_userptr (item, ptr + 1);
  return TRUE;
}

bool
item_decrease_refcount (ITEM *item)
{
  void *ptr = item_userptr (item);
  if (ptr == (void *) 0)
    return FALSE;
  set_item_userptr (item, ptr - 1);
  return TRUE;
}

int
item_get_refcount (ITEM *item)
{
  return (int) item_userptr (item);
}

SCM
gucu_new_item (SCM name, SCM description)
{
  SCM_ASSERT (scm_is_string (name), name, SCM_ARG1, "new-item");
  SCM_ASSERT (scm_is_string (description), description, SCM_ARG2, "new-item");

  char *c_name = scm_to_locale_string (name);
  char *c_description = scm_to_locale_string (description);

  ITEM *c_item = new_item (c_name, c_description);
  if (c_item == NULL)
    {
      if (errno == E_BAD_ARGUMENT)
        {
          scm_error_scm (scm_from_locale_symbol ("ncurses"),
                         scm_from_locale_string ("new-item"),
                         scm_from_locale_string ("bad argument"),
                         SCM_BOOL_F, SCM_BOOL_F);
        }
      else if (errno == E_SYSTEM_ERROR)
        {
          scm_error_scm (scm_from_locale_symbol ("ncurses"),
                         scm_from_locale_string ("new-item"),
                         scm_from_locale_string ("system error"),
                         SCM_BOOL_F, SCM_BOOL_F);
        }
      else
        abort ();
    }

  /* This is a new item, so its refcount should be one. */
  item_init_refcount (c_item);

  SCM ret = _scm_from_item (c_item);

  return ret;
}


int
_scm_is_item (SCM x)
{
  if (SCM_IS_A_P (x, item_fo_type))
    {
      if (scm_foreign_object_ref (x, 0) == NULL)
        return 0;
      else
        return 1;
    }
  else
    return 0;
}

ITEM *
_scm_to_item (SCM x)
{
  return (ITEM *) scm_foreign_object_ref (x, 0);
}

SCM
_scm_from_item (ITEM *x)
{
  SCM s_item;

  assert (x != NULL);

  s_item = scm_make_foreign_object_1 (item_fo_type, x);

  assert (x == (ITEM *) scm_foreign_object_ref (s_item, 0));

  if (0)
    {
      fprintf (stderr, "Making <#item> foreign object from ITEM * %p\n",
               (void *) x);
    }

  return (s_item);
}

void
gc_free_item (SCM item)
{
  ITEM *m = _scm_to_item (item);
  if (m != NULL)
    {
      item_decrease_refcount (m);
      if (item_get_refcount (m) == 0)
        {
          // Since no other #<menu> or #<item> is using the underlying
          // ITEM *, we can free it.

          // Discarding this const qualifier below is intentional
          free (item_name (m));
          free (item_description (m));
          free_item (m);
          scm_foreign_object_set_x (item, 0, NULL);
        }
      else
        {
          // Since some other #<menu> or #<item> has a reference to
          // this ITEM *, we just detach it.
          scm_foreign_object_set_x (item, 0, NULL);
        }
    }
}

SCM
gucu_is_item_p (SCM x)
{
  return scm_from_bool (_scm_is_item (x));
}

SCM
gucu_item_equal_p (SCM item1, SCM item2)
{
  return scm_from_bool (_scm_to_item (item1) == _scm_to_item (item2));
}

SCM
gucu_item_refcount (SCM x)
{
  SCM_ASSERT (_scm_is_item (x), x, SCM_ARG1, "%item-refcount");

  ITEM *m = _scm_to_item (x);
  return scm_from_int (item_get_refcount (m));
}

// menu -- in C, a MENU *.  In Scheme, a foreign object that contains
// the pointer to a form along with a list that contains the SCM of
// the fields

// Note the C Menu item's internal list of fields must match the SCM
// list of field to avoid garbage collection madness.

int
_scm_is_menu (SCM x)
{
  if (SCM_IS_A_P (x, menu_fo_type))
    {
      if (scm_foreign_object_ref (x, 0) == NULL)
        return 0;
      else
        return 1;
    }
  else
    return 0;
}

MENU *
_scm_to_menu (SCM x)
{
  scm_assert_foreign_object_type (menu_fo_type, x);
  return (MENU *) scm_foreign_object_ref (x, 0);
}

SCM
gucu_menu_equal_p (SCM menu1, SCM menu2)
{
  return scm_from_bool (_scm_to_menu (menu1) == _scm_to_menu (menu2));
}

// There is no _scm_from_menu since we need its items as a list to do
// that
// SCM _scm_from_menu (MENU *x)

/* The name is gc_free_menu because the curses primitive that frees
   memory is called free_menu. */
void
gc_free_menu (SCM x)
{
  int retval;
  int i;

  MENU *menu = (MENU *) scm_foreign_object_ref (x, 0);
  if (menu != NULL)
    {
      // First, we need make our own store of the ITEM *.  If free_menu
      // succeeds, the list of ITEM * provided by menu_items will no
      // longer be valid.
      int len = item_count (menu);
      ITEM **pitem = menu_items (menu);
      ITEM **pitem_store = scm_malloc (sizeof (ITEM *) * len);
      for (i = 0; i < len; i++)
        pitem_store[i] = pitem[i];

      // Next, we try to free the menu.  Note that if the menu freeing
      // is successful, ncurses will modify this menu's items to no
      // longer be connected to the menu, but, it won't free them.

      int free_attempts = 0;
    freemenu:
      retval = free_menu (menu);

      if (retval == E_BAD_ARGUMENT)
        {
          free (pitem_store);
          scm_misc_error ("garbage collection of menu", "bad argument",
                          SCM_EOL);
        }
      else if (retval == E_SYSTEM_ERROR)
        {
          free (pitem_store);
          scm_misc_error ("garbage collection of menu", "system error",
                          SCM_EOL);
        }
      else if (retval == E_POSTED)
        {
          // If we get the E_POSTED error, this menu is being garbage
          // collected without someone having properly called
          // unpost_menu() first.  We'll try to recover from that
          // mistake here.
          if (free_attempts == 0)
            {
              unpost_menu (menu);
              free_attempts++;
              goto freemenu;
            }
          else
            {
              free (pitem_store);
              scm_misc_error ("garbage collection of menu", "posted",
                              SCM_EOL);
            }
        }

      // If we get this far, the menu is now detached from the menu items.

      // It is a violation of the Ncurses Menu library to keep using an item
      // after the menu is gone, but, scheme programmers expect to be
      // able to use menu items and menus separately.  When a menu
      // is freed, its menu items still hold references to one another
      // to organize themselves in a doubly linked list, which frustrates
      // garbage collection.  This doubly-linked list deteched here.
      for (i = 0; i < len; i++)
        {
          pitem_store[i]->left = NULL;
          pitem_store[i]->right = NULL;
          pitem_store[i]->up = NULL;
          pitem_store[i]->down = NULL;
        }

      // Decrease the refcount on these items, and maybe free them.
      for (i = 0; i < len; i++)
        {
          if (!item_decrease_refcount (pitem_store[i]))
            {
              // Supposed to be impossible to hit this error
              scm_misc_error ("garbage collection of menu",
                              "refcount underflow", SCM_EOL);
            }
          if (item_get_refcount (pitem_store[i]) == 0)
            {
              free (item_name (pitem_store[i]));
              free (item_description (pitem_store[i]));
              free_item (pitem_store[i]);
            }
        }

      // Free our storage of the ITEM *
      free (pitem_store);

      scm_foreign_object_set_x (x, 0, NULL);
    }

  /* Release the references holding the window and subwindow. */
  scm_foreign_object_set_x (x, 1, SCM_UNPACK_POINTER (SCM_BOOL_F));
  scm_foreign_object_set_x (x, 2, SCM_UNPACK_POINTER (SCM_BOOL_F));
}

SCM
gucu_is_menu_p (SCM x)
{
  return scm_from_bool (_scm_is_menu (x));
}

SCM
gucu_new_menu (SCM items)
{
  size_t len;
  ITEM **c_items;
  SCM fobj;
  SCM entry;
  size_t i;

  /* Step 0: Check input list */
  SCM_ASSERT (scm_is_true (scm_list_p (items)), items, SCM_ARG1, "new-menu");

  len = scm_to_size_t (scm_length (items));
  if (len == 0)
    {
      scm_wrong_type_arg ("new-menu", SCM_ARG1, items);
      return (SCM_UNSPECIFIED);
    }
  for (i = 0; i < len; i++)
    {
      entry = scm_list_ref (items, scm_from_int (i));
      if (!_scm_is_item (entry))
        scm_wrong_type_arg ("new-menu", SCM_ARG1, items);
    }
  for (i = 0; i < len; i++)
    {
      entry = scm_list_ref (items, scm_from_int (i));
      ITEM *it = _scm_to_item (entry);
      if (item_index (it) != ERR)
        scm_error_scm (scm_from_locale_symbol ("ncurses"),
                       scm_from_locale_string ("new-menu"),
                       scm_from_locale_string
                       ("~A is already assigned to a menu"),
                       scm_list_1 (entry), SCM_BOOL_F);
    }

  // Step 1: allocate memory
  c_items = malloc (sizeof (ITEM *) * (len + 1));

  // Step 2: initialize it with C code

  // Step 3: create the foreign object
  fobj =
    scm_make_foreign_object_3 (menu_fo_type, NULL,
                               SCM_UNPACK_POINTER (SCM_BOOL_F),
                               SCM_UNPACK_POINTER (SCM_BOOL_F));

  // Step 4: finish the initialization
  for (i = 0; i < len; i++)
    {
      entry = scm_list_ref (items, scm_from_int (i));
      c_items[i] = _scm_to_item (entry);
      if (!item_increase_refcount (c_items[i]))
        {
          // Zero out this array so that it can be garbage collected.
          memset (c_items, 0, (len + 1) * sizeof (ITEM *));

          scm_misc_error ("new-menu", "too many references on item ~s",
                          scm_list_1 (entry));
        }
    }

  /* This is a null-terminated array */
  c_items[len] = NULL;

  MENU *menu = new_menu (c_items);

  if (menu == NULL)
    {
      free (c_items);
      if (errno == E_NOT_CONNECTED)
        {
          scm_misc_error ("new-menu", "menu has no items", SCM_EOL);
        }
      else if (errno == E_SYSTEM_ERROR)
        {
          scm_error_scm (scm_from_locale_symbol ("ncurses"),
                         scm_from_locale_string ("new-menu"),
                         scm_from_locale_string ("system error"),
                         SCM_BOOL_F, SCM_BOOL_F);
        }
      else
        abort ();
    }
  scm_foreign_object_set_x (fobj, 0, menu);

  return fobj;
}

#define makeFO(a,b,c) scm_make_foreign_object_type((a),(b),(c))
#define u8sym(x) scm_from_utf8_symbol(x)
void
gucu_menu_init_type ()
{
  item_fo_type = makeFO (u8sym ("item"),
                         scm_list_1 (u8sym ("data")), gc_free_item);
  scm_c_define_gsubr ("item?", 1, 0, 0, gucu_is_item_p);
  scm_c_define_gsubr ("item=?", 2, 0, 0, gucu_item_equal_p);
  scm_c_define_gsubr ("new-item", 2, 0, 0, gucu_new_item);
  scm_c_define_gsubr ("%item-refcount", 1, 0, 0, gucu_item_refcount);

  menu_fo_type = makeFO (u8sym ("menu"),
                         scm_list_3 (u8sym ("menu"), u8sym ("window"),
                                     u8sym ("subwindow")), gc_free_menu);
  scm_c_define_gsubr ("menu?", 1, 0, 0, gucu_is_menu_p);
  scm_c_define_gsubr ("menu=?", 2, 0, 0, gucu_menu_equal_p);
  scm_c_define_gsubr ("new-menu", 1, 0, 0, gucu_new_menu);
}
