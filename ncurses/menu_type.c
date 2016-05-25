/*
  menu_type.c

  Copyright 2009, 2010, 2011, 2014, 2016 Free Software Foundation, Inc.

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

#include "compat.h"
#include "menu_type.h"
#include "type.h"

scm_t_bits menu_tag;
scm_t_bits item_tag;

SCM equalp_item (SCM x1, SCM x2);
size_t gc_free_item (SCM x);
SCM mark_item (SCM x);
int print_item (SCM x, SCM port, scm_print_state * pstate);

SCM equalp_menu (SCM x1, SCM x2);
size_t gc_free_menu (SCM x);
SCM mark_menu (SCM x);
int print_menu (SCM x, SCM port, scm_print_state * pstate);

/* item -- in C, an ITEM *.  In Scheme, a smob that contains the
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
  if (SCM_SMOB_PREDICATE (item_tag, x))
    {
      if (SCM_SMOB_DATA (x) == 0)
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
  return (ITEM *) SCM_SMOB_DATA (x);
}

SCM
_scm_from_item (ITEM * x)
{
  SCM s_item;

  assert (x != NULL);

  SCM_NEWSMOB (s_item, item_tag, x);

  assert (x == (ITEM *) SCM_SMOB_DATA (s_item));

  if (0)
    {
      fprintf (stderr, "Making <#item> smob from ITEM * %p\n", (void *) x);
    }

  return (s_item);
}


// Items are equal if they point to the same C structure
SCM
equalp_item (SCM x1, SCM x2)
{
  ITEM *item1 = (ITEM *) SCM_SMOB_DATA (x1);
  ITEM *item2 = (ITEM *) SCM_SMOB_DATA (x2);

  if ((item1 == NULL) || (item2 == NULL))
    return SCM_BOOL_F;
  else if ((item1 != item2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_item (SCM x UNUSED)
{
  // No SCMs in the item type: nothing to do here.
  return (SCM_BOOL_F);
}

size_t
gc_free_item (SCM item)
{
  SCM_ASSERT (_scm_is_item (item), item, SCM_ARG1, "free-item");

  ITEM *m = _scm_to_item (item);
  if (m != NULL)
    {
      item_decrease_refcount (m);
      if (item_get_refcount (m) == 0)
	{
	  // Since no other #<menu> or #<item> is using the underlying
	  // ITEM *, we can free it.
	  free (item_name (m));
	  free (item_description (m));
	  free_item (m);
	  SCM_SET_SMOB_DATA (item, 0);
	}
      else
	{
	  // Since some other #<menu> or #<item> has a reference to
	  // this ITEM *, we just detach it.
	  SCM_SET_SMOB_DATA (item, 0);
	}
    }

  return 0;
}

int
print_item (SCM x, SCM port, scm_print_state * pstate UNUSED)
{
  ITEM *item = (ITEM *) SCM_SMOB_DATA (x);
  char str[SIZEOF_VOID_P*2+3];

  assert (item != NULL);

  scm_puts ("#<item ", port);

  if (item == (ITEM *) NULL)
    scm_puts ("(freed)", port);
  else
    {
      if (item_name (item) != NULL)
        {
          scm_puts (item_name (item), port);
          scm_puts (" ", port);
        }

      if (snprintf (str, sizeof(str), "%p", (void *) item) < 0)
        scm_puts ("???", port);
      else
        scm_puts (str, port);
    }
  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_item_p (SCM x)
{
  return scm_from_bool (_scm_is_item (x));
}

SCM
gucu_item_refcount (SCM x)
{
  SCM_ASSERT (_scm_is_item (x), x, SCM_ARG1, "%item-refcount");

  ITEM *m = _scm_to_item (x);
  return scm_from_int (item_get_refcount (m));
}

// menu -- in C, a MENU *.  In Scheme, a smob that contains the pointer
// to a form along with a list that contains the SCM of the fields

// Note the C Menu item's internal list of fields must match the SCM
// list of field to avoid garbage collection madness.

int
_scm_is_menu (SCM x)
{
  if (SCM_SMOB_PREDICATE (menu_tag, x))
    {
      if (SCM_SMOB_DATA (x) == 0)
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
  struct gucu_menu *gm;

  scm_assert_smob_type (menu_tag, x);

  gm = (struct gucu_menu *) SCM_SMOB_DATA (x);
  if (gm == NULL)
    return NULL;

  return gm->menu;
}

// There is no _scm_from_menu since we need its items as a list to do
// that
// SCM _scm_from_menu (MENU *x)

// Menus are equal if they point to the same C structure
SCM
equalp_menu (SCM x1, SCM x2)
{
  MENU *menu1 = _scm_to_menu (x1);
  MENU *menu2 = _scm_to_menu (x2);

  if ((menu1 == NULL) || (menu2 == NULL))
    return SCM_BOOL_F;
  else if ((menu1 != menu2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_menu (SCM x)
{
  struct gucu_menu *gm;

  scm_assert_smob_type (menu_tag, x);

  gm = (struct gucu_menu *) SCM_SMOB_DATA (x);
  if (gm != NULL)
    {
      scm_gc_mark (gm->win_guard);
      scm_gc_mark (gm->subwin_guard);
    }
  return SCM_BOOL_F;
}

/* The name is gc_free_menu because the curses primitive that frees
   memory is called free_menu. */
size_t
gc_free_menu (SCM x)
{
  struct gucu_menu *gm;
  int retval;

  scm_assert_smob_type (menu_tag, x);

  gm = (struct gucu_menu *) SCM_SMOB_DATA (x);
  if (gm != NULL && gm->menu != NULL)
    {
      // First, we need make our own store the ITEM *.  If free_menu
      // succeeds, the list of ITEM * provided by menu_items will no
      // longer be valid.
      int len = item_count (gm->menu);
      ITEM **pitem = menu_items (gm->menu);
      ITEM **pitem_store = scm_malloc (sizeof (ITEM *) * len);
      for (int i = 0; i < len; i ++)
	pitem_store[i] = pitem[i];

      // Next, we try to free the menu.  Note that if the menu freeing
      // is successful, ncurses will modify this menu's items to no
      // longer be connected to the menu, but, it won't free them.

      retval = free_menu (gm->menu);

      if (retval == E_BAD_ARGUMENT)
	{
	  free (pitem_store);
	  scm_misc_error ("garbage collection of menu", "bad argument", SCM_EOL);
	}
      else if (retval == E_SYSTEM_ERROR)
	{
	  free (pitem_store);
	  scm_misc_error ("garbage collection of menu", "system error", SCM_EOL);
	}
      else if (retval == E_POSTED)
	{
	      free (pitem_store);
	      scm_misc_error ("garbage collection of menu", "posted", SCM_EOL);
	}

      // If we get this far, the menu is now detached from the menu items.
      // Decrease the refcount on these items, and maybe free them.
      for (int i = 0; i < len; i ++)
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

      gm->menu = NULL;
    }

  /* Release the references holding the windows. */
  if (gm != NULL)
    {
      gm->win_guard = SCM_BOOL_F;
      gm->subwin_guard = SCM_BOOL_F;
    }

  SCM_SET_SMOB_DATA (x, NULL);

  return 0;
}

int
print_menu (SCM x, SCM port, scm_print_state * pstate UNUSED)
{
  MENU *menu = _scm_to_menu (x);
  char str[SIZEOF_VOID_P*2+3];

  scm_puts ("#<menu ", port);
  if (menu == NULL)
    {
      scm_puts ("(freed) ", port);
    }
  else
    {
      if (snprintf (str, sizeof(str), "%p", (void *) menu) < 0)
	scm_puts ("???", port);
      else
	scm_puts (str, port);
    }
  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_menu_p (SCM x)
{
  return scm_from_bool (_scm_is_menu (x));
}

SCM
gucu_new_menu (SCM items)
{
  struct gucu_menu *gm;
  size_t len;
  ITEM **c_items;
  SCM smob;
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
                       scm_from_locale_string ("~A is already assigned to a menu"),
                       scm_list_1 (entry),
                       SCM_BOOL_F);
    }

  // Step 1: allocate memory
  gm = scm_gc_malloc (sizeof (struct gucu_menu), "gucu_menu");

  c_items = scm_gc_malloc (sizeof (ITEM *) * (len + 1), "gucu_menu");

  // Step 2: initialize it with C code

  // Step 3: create the smob
  SCM_NEWSMOB (smob, menu_tag, gm);

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

  gm->menu = new_menu (c_items);

  if (gm->menu == NULL)
    {
      free (c_items);
      if (errno == E_NOT_CONNECTED)
        {
          scm_misc_error ("new-menu", "menu has no items", SCM_BOOL_F);
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
  scm_remember_upto_here_1 (items);

  gm->win_guard = SCM_BOOL_F;
  gm->subwin_guard = SCM_BOOL_F;

  /* Guard the items list */
  return smob;
}

void
gucu_menu_init_type ()
{
  item_tag = scm_make_smob_type ("item", sizeof (ITEM *));
  scm_set_smob_mark (item_tag, mark_item);
  scm_set_smob_free (item_tag, gc_free_item);
  scm_set_smob_print (item_tag, print_item);
  scm_set_smob_equalp (item_tag, equalp_item);
  scm_c_define_gsubr ("item?", 1, 0, 0, gucu_is_item_p);
  scm_c_define_gsubr ("new-item", 2, 0, 0, gucu_new_item);
  scm_c_define_gsubr ("%item-refcount", 1, 0, 0, gucu_item_refcount);

  menu_tag = scm_make_smob_type ("menu", sizeof (struct menu *));
  scm_set_smob_mark (menu_tag, mark_menu);
  scm_set_smob_free (menu_tag, gc_free_menu);
  scm_set_smob_print (menu_tag, print_menu);
  scm_set_smob_equalp (menu_tag, equalp_menu);
  scm_c_define_gsubr ("menu?", 1, 0, 0, gucu_is_menu_p);
  scm_c_define_gsubr ("new-menu", 1, 0, 0, gucu_new_menu);
}
