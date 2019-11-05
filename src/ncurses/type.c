/*
  type.c

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

#define _GNU_SOURCE
#include <config.h>

#include <assert.h>
#include <errno.h>
#include <libguile.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#if HAVE_CURSES_H
#include <curses.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/curses.h>
#else
#error "No curses.h file included"
#endif

#include "gucuconfig.h"
#include "type.h"
#include "unicode.h"

/* The maximum number of characters in a complex character */
#ifdef HAVE_NCURSESW
#define GUCU_CCHARW_MAX (CCHARW_MAX)
#else
#define GUCU_CCHARW_MAX (5)
#endif

static SCM screen_fo_type;
SCM window_fo_type;

static void gc_free_screen (SCM x);

static SCM gucu_window_equalp (SCM x1, SCM x2);
static void gc_free_window (SCM x);

/* attr -- character attributes, bit flags packed into an unsigned:
   probably uint32 */

int
_scm_is_attr (SCM x)
{
  return scm_is_integer (x);
}

attr_t
_scm_to_attr (SCM x)
{
  if (SIZEOF_INT == SIZEOF_ATTR_T)
    return (attr_t) scm_to_uint (x);
  else if (SIZEOF_LONG == SIZEOF_ATTR_T)
    return (attr_t) scm_to_ulong (x);
  else
    abort ();
}

SCM
_scm_from_attr (attr_t x)
{
  if (SIZEOF_INT == SIZEOF_ATTR_T)
    return scm_from_uint (x);
  else if (SIZEOF_LONG == SIZEOF_ATTR_T)
    return scm_from_ulong (x);
  else
    abort ();
}


/*
  CHARACTERS

  xchar: a wide character with possible combining characters and its
  associated renditions.  In C, a cchar_t struct.  In scheme, a list
  where element 0 is the attributes, element 1 is the color pair, and
  the rest of the list is the code points of the character and its
  accents.
*/

int
_scm_is_xchar (SCM x)
{
  int i, len;

  if (!scm_is_true (scm_list_p (x)))
    return 0;

  len = scm_to_int (scm_length (x));

  if (len > 2 + GUCU_CCHARW_MAX
      || !_scm_is_attr (scm_list_ref (x, scm_from_int (0)))
      || !scm_is_integer (scm_list_ref (x, scm_from_int (1))))
    return 0;

  for (i = 2; i < len; i++)
    if (!SCM_CHARP (scm_list_ref (x, scm_from_int (i))))
      return 0;

  return 1;
}

#ifdef HAVE_NCURSESW
/* Converts a wide NCurses complex character structure to a GuCu
   complex character */
SCM
_scm_xchar_from_cchar (cchar_t * x)
{
  int i;
  int len;
  int ret;
  wchar_t wch[10];
  attr_t attr;
  short color_pair;
  SCM element;
  SCM element_list;
  SCM total_list = SCM_EOL;

  assert (x != NULL);

  len = getcchar (x, 0, 0, 0, 0);
  /* Starting from the patch on 2009/07/18, the length returned by
     getcchar includes the trailing NULL.  Prior to that, it did not
     include the trailing NULL. */
  if (NCURSES_VERSION_MAJOR > 5
      || (NCURSES_VERSION_MAJOR == 5 && NCURSES_VERSION_MINOR > 7)
      || (NCURSES_VERSION_MAJOR == 5 && NCURSES_VERSION_MINOR == 7
          && NCURSES_VERSION_PATCH >= 20090718))
    {
      len--;
    }

  ret = getcchar (x, wch, &attr, &color_pair, NULL);

  if (ret == ERR)
    scm_misc_error (NULL, "error unpacking complex char", SCM_EOL);

  /* Strip the color info from attr */
  attr &= A_ATTRIBUTES ^ A_COLOR;

  total_list =
    scm_list_2 (_scm_from_attr (attr), scm_from_short (color_pair));

  for (i = 0; i < len; i++)
    {
#ifdef GUILE_CHARS_ARE_UCS4
      {
        uint32_t cp;
        ret = wchar_to_codepoint (wch[i], &cp);
        if (ret == 0)
          element = SCM_MAKE_CHAR (GUCU_REPLACEMENT_CODEPOINT);
        else
          element = SCM_MAKE_CHAR (cp);
      }
#else
      {
        int b = wctob ((wint_t) wch[i]);
        if (b == EOF)
          element = SCM_MAKE_CHAR (GUCU_REPLACEMENT_CHAR);
        else
          element = SCM_MAKE_CHAR ((unsigned char) b);
      }
#endif
      element_list = scm_list_1 (element);
      total_list = scm_append (scm_list_2 (total_list, element_list));
    }

  return total_list;
}
#endif

/* Converts a Curses rendered character to a GuCu complex character */
SCM
_scm_xchar_from_chtype (chtype x)
{
  unsigned char c;
  attr_t attr;
  short color_pair;
  SCM total_list = SCM_EOL;

  attr = x & (A_ATTRIBUTES ^ A_COLOR);
  color_pair = PAIR_NUMBER (x);
  c = x & A_CHARTEXT;

#ifdef GUILE_CHARS_ARE_UCS4
  {
    int ret;
    uint32_t cp;
    ret = locale_char_to_codepoint (c, &cp);
    if (!ret)
      total_list =
        scm_list_3 (_scm_from_attr (attr), scm_from_short (color_pair),
                    SCM_MAKE_CHAR (GUCU_REPLACEMENT_CODEPOINT));
    else
      total_list =
        scm_list_3 (_scm_from_attr (attr), scm_from_short (color_pair),
                    SCM_MAKE_CHAR (cp));
  }
#else
  total_list = scm_list_3 (_scm_from_attr (attr), scm_from_short (color_pair),
                           SCM_MAKE_CHAR (c));
#endif

  return total_list;

}

/* Converts an 8-bit locale-encoded character to a Guile character */
SCM
_scm_schar_from_char (char c)
{
#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  uint32_t cp;

  ret = locale_char_to_codepoint (c, &cp);
  if (!ret)
    return SCM_BOOL_F;

  return SCM_MAKE_CHAR (cp);
#else
  return SCM_MAKE_CHAR (c);
#endif
}

/* Converts a libc wide character to a Guile characters */
SCM
_scm_schar_from_wchar (wchar_t ch)
{
#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  uint32_t cp;

  ret = wchar_to_codepoint (ch, &cp);
  if (ret == 0)
    return SCM_BOOL_F;

  return SCM_MAKE_CHAR (cp);
#else
  int b = wctob ((wint_t) ch);
  if (b == EOF)
    return SCM_BOOL_F;

  return SCM_MAKE_CHAR ((unsigned char) b);
#endif
}

#ifdef HAVE_NCURSESW
/* Converts a GuCu complex character to a wide NCurses complex character */
cchar_t *
_scm_xchar_to_cchar (SCM x)
{
  int i;
  SCM member;
  wchar_t wch[GUCU_CCHARW_MAX + 1];

  cchar_t *cchar = (cchar_t *) scm_malloc (sizeof (cchar_t));
  int len = scm_to_int (scm_length (x));
  attr_t attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
  short color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));

  assert (_scm_is_xchar (x));

  for (i = 2; i < len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));

#ifdef GUILE_CHARS_ARE_UCS4
      {
        int ret;
        uint32_t codepoint;
        wchar_t wc;

        codepoint = SCM_CHAR (member);
        ret = codepoint_to_wchar (codepoint, &wc);
        if (ret)
          {
            wch[i - 2] = wc;
          }
        else
          {
            wch[i - 2] = GUCU_REPLACEMENT_WCHAR;
            wch[i - 1] = L'\0';
            break;
          }
      }
#else
      {
        wint_t wint;
        wint = btowc ((int) SCM_CHAR (member));
        if (wint == WEOF)
          {
            wch[i - 2] = GUCU_REPLACEMENT_WCHAR;
            wch[i - 1] = L'\0';
            break;
          }
        else
          {
            wch[i - 2] = (wchar_t) wint;
          }
      }
#endif
    }
  wch[len - 2] = L'\0';

  if (OK != setcchar (cchar, wch, attr, color_pair, NULL))
    {
      return (cchar_t *) NULL;
    }

  return cchar;
}
#endif

chtype
_scm_xchar_to_chtype (SCM x)
{
  chtype ch;
  attr_t attr;
  short color_pair;
  char c;

#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  uint32_t codepoint;

  assert (_scm_is_xchar (x));

  codepoint = SCM_CHAR (scm_list_ref (x, scm_from_int (2)));
  ret = codepoint_to_locale_char (codepoint, &c);
  if (ret)
    ch = (chtype) (unsigned char) c;
  else
    ch = (chtype) (unsigned char) GUCU_REPLACEMENT_CHAR;
  attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
  color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));
  ch = ch | attr | COLOR_PAIR (color_pair);

#else
  assert (_scm_is_xchar (x));

  c = SCM_CHAR (scm_list_ref (x, scm_from_int (2)));
  attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
  color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));
  ch = (chtype) (unsigned char) c | attr | COLOR_PAIR (color_pair);
#endif

  return ch;

}

wchar_t
_scm_schar_to_wchar (SCM x)
{
#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  wchar_t c;
  uint32_t codepoint;

  assert (SCM_CHARP (x));

  codepoint = SCM_CHAR (x);
  ret = codepoint_to_wchar (codepoint, &c);
  if (!ret)
    return GUCU_REPLACEMENT_WCHAR;

  return c;
#else
  wint_t wc;

  assert (SCM_CHARP (x));
  wc = btowc ((int) SCM_CHAR (x));
  if (wc == WEOF)
    return GUCU_REPLACEMENT_WCHAR;

  return (wchar_t) wc;
#endif
}

char
_scm_schar_to_char (SCM x)
{
#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  char c;
  uint32_t codepoint;

  assert (SCM_CHARP (x));

  codepoint = SCM_CHAR (x);
  ret = codepoint_to_locale_char (codepoint, &c);
  if (!ret)
    return GUCU_REPLACEMENT_CHAR;

  return c;
#else
  assert (SCM_CHARP (x));
  return SCM_CHAR (x);
#endif
}

SCM
gucu_schar_from_char (SCM c)
{
  int c_c;
  SCM_ASSERT (scm_is_integer (c), c, SCM_ARG1, "%scheme-char-from-c-char");
  c_c = scm_to_int (c);
  return _scm_schar_from_char ((char) (unsigned char) c_c);
}

SCM
gucu_schar_to_char (SCM c)
{
  char c_c;
  SCM_ASSERT (SCM_CHARP (c), c, SCM_ARG1, "%scheme-char-to-c-char");
  c_c = _scm_schar_to_char (c);
  return scm_from_uint ((unsigned char) c_c);
}

SCM
gucu_schar_from_wchar (SCM wc)
{
  wchar_t c_wc;
  SCM_ASSERT (scm_is_integer (wc), wc, SCM_ARG1, "%scheme-char-from-c-wchar");
  c_wc = scm_to_uint (wc);
  return _scm_schar_from_wchar (c_wc);
}

SCM
gucu_schar_to_wchar (SCM c)
{
  wchar_t c_c;
  SCM_ASSERT (SCM_CHARP (c), c, SCM_ARG1, "%scheme-char-to-c-wchar");
  c_c = _scm_schar_to_wchar (c);
  return scm_from_uint (c_c);
}

SCM
gucu_xchar_from_chtype (SCM c)
{
  SCM_ASSERT (_scm_is_chtype (c), c, SCM_ARG1, "%xchar-from-chtype");
  return _scm_xchar_from_chtype (_scm_to_chtype (c));
}

SCM
gucu_xchar_to_chtype (SCM c)
{
  SCM_ASSERT (_scm_is_xchar (c), c, SCM_ARG1, "%xchar-to-chtype");
  return _scm_from_chtype (_scm_xchar_to_chtype (c));
}

///////////////////////////////////
// STRINGS

// Guile strings are either standard strings, a list of chars, or a
// list of cchars

int
_scm_is_xstring (SCM x)
{
  if (scm_is_true (scm_list_p (x)))
    {
      int i, len;

      len = scm_to_int (scm_length (x));
      for (i = 0; i < len; i++)
        {
          if (!_scm_is_xchar (scm_list_ref (x, scm_from_int (i))))
            return 0;
        }
    }
  return 1;
}

#ifdef HAVE_NCURSESW
SCM
_scm_sstring_from_wint_string (const wint_t * x)
{
  int i, len;
  SCM member, xstring;

  assert (x != NULL);

  len = 0;
  while (x[len] != 0)
    len++;
  xstring = SCM_EOL;
  for (i = 0; i < len; i++)
    {
      if (x[i] <= WCHAR_MAX)
        member = _scm_schar_from_wchar (x[i]);
      else
        member = SCM_MAKE_CHAR (GUCU_REPLACEMENT_CODEPOINT);
      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
    }

  return scm_string (xstring);
}
#endif

#ifdef HAVE_NCURSESW
SCM
_scm_sstring_from_wstring (const wchar_t *x)
{
  size_t i;
  SCM member, xstring;

  assert (x != NULL);

  xstring = SCM_EOL;
  for (i = 0; i < wcslen (x); i++)
    {
      member = _scm_schar_from_wchar (x[i]);
      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
    }

  return scm_string (xstring);
}
#endif

char *
_scm_sstring_to_locale_string (SCM x)
{
  assert (scm_is_string (x));

  return scm_to_locale_string (x);
}

wchar_t *
_scm_sstring_to_wstring (SCM x)
{
  size_t i, len;
  SCM member;
  wchar_t *wstring;
  wchar_t wchar;

  assert (scm_is_string (x));

  len = scm_c_string_length (x);
  wstring = (wchar_t *) scm_malloc (sizeof (wchar_t) * (len + 1));

  for (i = 0; i < len; i++)
    {
      member = scm_c_string_ref (x, i);
      wchar = _scm_schar_to_wchar (member);
      memcpy (wstring + i, &wchar, sizeof (wchar_t));
    }
  wstring[len] = L'\0';

  return wstring;
}

SCM
_scm_xstring_from_chstring (const chtype *x)
{
  size_t i;
  SCM member, xstring;

  assert (x != NULL);

  xstring = SCM_EOL;
  i = 0;
  while (1)
    {
      if (x[i] == 0)
        break;
      member = _scm_xchar_from_chtype (x[i]);
      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
      i++;
    }

  return xstring;
}


#ifdef HAVE_NCURSESW
SCM
_scm_xstring_from_cstring (const cchar_t * x)
{
  int i, n;
  SCM member, xstring;
  wchar_t wch[GUCU_CCHARW_MAX];
  attr_t attrs;
  short color_pair;

  assert (x != NULL);

  xstring = SCM_EOL;
  i = 0;
  while (1)
    {
      if (x[i].chars[0] == 0)
        break;
      n = getcchar (&(x[i]), NULL, NULL, NULL, NULL);
      /* Starting from the patch on 2009/07/18, the length returned by
         getcchar includes the trailing NULL.  Prior to that, it did not
         include the trailing NULL. */

      if (NCURSES_VERSION_MAJOR > 5
          || (NCURSES_VERSION_MAJOR == 5 && NCURSES_VERSION_MINOR > 7)
          || (NCURSES_VERSION_MAJOR == 5 && NCURSES_VERSION_MINOR == 7
              && NCURSES_VERSION_PATCH >= 20090718))
        {
          n--;
        }

      if (n == 0)
        break;
      getcchar (&(x[i]), wch, &attrs, &color_pair, NULL);
      if (n == 1)
        member = scm_list_3 (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]));

      else if (n == 2)
        member = scm_list_4 (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]),
                             _scm_schar_from_wchar (wch[1]));
      else if (n == 3)
        member = scm_list_5 (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]),
                             _scm_schar_from_wchar (wch[1]),
                             _scm_schar_from_wchar (wch[2]));
      else if (n == 4)
        member = scm_list_n (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]),
                             _scm_schar_from_wchar (wch[1]),
                             _scm_schar_from_wchar (wch[2]),
                             _scm_schar_from_wchar (wch[3]), SCM_UNDEFINED);

      else if (n == 5)
        member = scm_list_n (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]),
                             _scm_schar_from_wchar (wch[1]),
                             _scm_schar_from_wchar (wch[2]),
                             _scm_schar_from_wchar (wch[3]),
                             _scm_schar_from_wchar (wch[4]), SCM_UNDEFINED);
      else
        abort ();

      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
      i++;
    }

  return xstring;
}
#endif

chtype *
_scm_xstring_to_chstring (SCM x)
{
  int i, len;
  SCM member;
  chtype *chstring;
  chtype ch;

  assert (_scm_is_xstring (x));

  len = scm_to_int (scm_length (x));
  chstring = (chtype *) scm_malloc (sizeof (chtype) * (len + 1));

  for (i = 0; i < len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));
      ch = _scm_xchar_to_chtype (member);
      memcpy (chstring + i, &ch, sizeof (chtype));
    }
  chstring[len] = 0;

  return chstring;
}

#ifdef HAVE_NCURSESW
cchar_t *
_scm_xstring_to_cstring (SCM x)
{
  int i, len;
  SCM member;
  cchar_t *cstring;
  cchar_t *cchar;
  static cchar_t terminator;
  static int first = 1;

  assert (_scm_is_xstring (x));

  if (first)
    {
      wchar_t wch = L'\0';
      setcchar (&terminator, &wch, A_NORMAL, 0, NULL);
      first = 0;
    }

  len = scm_to_int (scm_length (x));
  cstring = (cchar_t *) scm_malloc (sizeof (cchar_t) * (len + 1));

  for (i = 0; i < len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));
      cchar = _scm_xchar_to_cchar (member);
      memcpy (cstring + i, cchar, sizeof (cchar_t));
      free (cchar);
    }

  memcpy (cstring + len, &terminator, sizeof (cchar_t));

  return cstring;
}

#endif



// chtype -- in C, an integer that contains a characters and its rendition.
// In Scheme, an integer.

int
_scm_is_chtype (SCM x)
{
  return scm_is_integer (x);
}

chtype
_scm_to_chtype (SCM x)
{
  assert (_scm_is_chtype (x));

  if (SIZEOF_INT == SIZEOF_CHTYPE)
    return (chtype) scm_to_uint (x);
  else if (SIZEOF_LONG == SIZEOF_CHTYPE)
    return (chtype) scm_to_ulong (x);
  else
    abort ();
}

SCM
_scm_from_chtype (chtype x)
{
  if (SIZEOF_INT == SIZEOF_CHTYPE)
    return scm_from_uint (x);
  else if (SIZEOF_LONG == SIZEOF_CHTYPE)
    return scm_from_ulong (x);
  else
    abort ();
}

// mevent -- in C, a MEVENT.  In scheme, a list of 5 elements
int
_scm_is_mevent (SCM x)
{
  int err = 0;
  SCM member;
  int i;

  if (scm_is_true (scm_list_p (x)))
    {
      int len = scm_to_int (scm_length (x));

      for (i = 0; i < len; i++)
        {
          member = scm_list_ref (x, scm_from_int (i));
          if (!scm_is_integer (member))
            err++;
        }
    }
  else
    err++;

  if (err > 0)
    return 0;
  else
    return 1;
}

MEVENT *
_scm_to_mevent (SCM x)
{
  MEVENT *me;

  assert (_scm_is_mevent (x));

  me = (MEVENT *) malloc (sizeof (MEVENT));

  me->id = scm_to_short (scm_list_ref (x, scm_from_int (0)));
  me->x = scm_to_int (scm_list_ref (x, scm_from_int (1)));
  me->y = scm_to_int (scm_list_ref (x, scm_from_int (2)));
  me->z = scm_to_int (scm_list_ref (x, scm_from_int (3)));
  me->bstate = scm_to_ulong (scm_list_ref (x, scm_from_int (4)));

  return (me);
}

SCM
_scm_from_mevent (MEVENT * me)
{
  assert (me != NULL);

  return scm_list_5
    (scm_from_short (me->id),
     scm_from_int (me->x),
     scm_from_int (me->y), scm_from_int (me->z), scm_from_ulong (me->bstate));
}

SCM
gucu_is_mevent_p (SCM x)
{
  return scm_from_bool (_scm_is_mevent (x));
}


// screen -- in C, a SCREEN * and the associated input and output FILE
// * used to open it.  In Scheme, a pointer to a structure containing
// the same.

// The ncurses SCREEN type depends on file descriptors for input and
// output.  To make sure these file descriptors aren't closed or
// garbage collected prematurely, we carry references to them along
// with the SCREEN structure.  They must all exist as a set.

int
_scm_is_screen (SCM x)
{
  if (SCM_IS_A_P (x, screen_fo_type))
    {
      if (scm_foreign_object_ref (x, 0) != NULL)
        return 1;
      else
        return 0;
    }
  else
    return 0;
}

SCREEN *
_scm_to_screen (SCM x)
{
  assert (_scm_is_screen (x));
  return scm_foreign_object_ref (x, 0);
}

void
_scm_to_screen_and_ports (SCM x, SCREEN **screen, FILE **ifp, FILE **ofp)
{
  assert (_scm_is_screen (x));
  assert (screen != NULL);
  assert (ifp != NULL);
  assert (ofp != NULL);
  *screen = scm_foreign_object_ref (x, 0);
  *ifp = scm_foreign_object_ref (x, 1);
  *ofp = scm_foreign_object_ref (x, 2);
}

SCM
_scm_from_screen_and_ports (SCREEN *x, FILE *ifp, FILE *ofp)
{
  SCM s_screen;

  assert (x != NULL);
  assert (ifp != NULL);
  assert (ofp != NULL);

  s_screen = scm_make_foreign_object_3 (screen_fo_type, x, ifp, ofp);
  return s_screen;
}

/*  This procedure frees a screen without checking its type. In GC
 *  free procedures, the foreign object may have already forgotten its
 *  type. */
static void
gc_free_screen (SCM x)
{
  SCREEN *screen = scm_foreign_object_ref (x, 0);
  FILE *ifp = scm_foreign_object_ref (x, 1);
  FILE *ofp = scm_foreign_object_ref (x, 2);

  if (screen != NULL)
    {
      delscreen (screen);
      /* delscreen returns void */
      scm_foreign_object_set_x (x, 0, NULL);
    }
  if (ifp)
    {
      fclose (ifp);
      scm_foreign_object_set_x (x, 1, NULL);
    }
  if (ofp)
    {
      fclose (ofp);
      scm_foreign_object_set_x (x, 2, NULL);
    }
}

void
_scm_free_screen (SCM x)
{
  assert (_scm_is_screen (x));
  gc_free_screen (x);
}

SCM
gucu_is_screen_p (SCM x)
{
  return scm_from_bool (_scm_is_screen (x));
}

SCM
gucu_screen_equalp (SCM x, SCM y)
{
  return scm_from_bool (_scm_to_screen (x) == _scm_to_screen (y));
}

// window -- in C, a WINDOW *.  In Scheme, a foreign
// object that contains the pointer

int
_scm_is_window (SCM x)
{
  if (SCM_IS_A_P (x, window_fo_type))
    {
      if (scm_foreign_object_ref (x, 0) != NULL)
        return 1;
      else
        return 0;
    }
  else
    return 0;

}

WINDOW *
_scm_to_window (SCM x)
{
  assert (_scm_is_window (x));

  return scm_foreign_object_ref (x, 0);
}

SCM
_scm_from_window_full (SCM parent, SCM name, WINDOW *win)
{
  SCM s_win;

  assert (win != NULL);
  assert (_scm_is_window (parent) || scm_is_false (parent));
  assert (scm_is_string (name) || scm_is_false (name));
  void *vals[4] = { win, NULL, SCM_UNPACK_POINTER (parent),
    SCM_UNPACK_POINTER (name)
  };

  s_win = scm_make_foreign_object_n (window_fo_type, 4, vals);

  if (0)
    {
#ifdef NCURSES_OPAQUE
      fprintf (stderr, "Making foreign object from window\n");
#else
      fprintf (stderr, "Making foreign object from window at %d, %d\n",
               x->_begx, x->_begy);
#endif
    }

  return (s_win);
}

SCM
_scm_from_window (WINDOW *x)
{
  return _scm_from_window_full (SCM_BOOL_F, SCM_BOOL_F, x);
}

// Windows are equal if they point to the same C structures
static SCM
gucu_window_equalp (SCM x1, SCM x2)
{
  WINDOW *win1, *win2;

  SCM_ASSERT (_scm_is_window (x1), x1, SCM_ARG1, "window=?");
  SCM_ASSERT (_scm_is_window (x2), x1, SCM_ARG2, "window=?");

  win1 = _scm_to_window (x1);
  win2 = _scm_to_window (x2);

  if ((win1 == NULL) || (win2 == NULL))
    return SCM_BOOL_F;
  else if (win1 != win2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

static void
gc_free_window (SCM x)
{
  WINDOW *win = scm_foreign_object_ref (x, 0);
  /* Windows should already be null if delwin has been called on them */
  if (win != NULL)
    {
      // Detach the parent window, if any.
      scm_foreign_object_set_x (x, 2, SCM_UNPACK_POINTER (SCM_BOOL_F));
      // The window name
      scm_foreign_object_set_x (x, 3, SCM_UNPACK_POINTER (SCM_BOOL_F));
      PANEL *panel = scm_foreign_object_ref (x, 1);
      if (panel != NULL)
        {
          int retval;
          set_panel_userptr (panel, NULL);
          retval = del_panel (panel);
          if (retval != OK)
            {
              scm_error_scm (scm_from_locale_symbol ("ncurses"),
                             scm_from_locale_string
                             ("garbage collection of panel"),
                             scm_from_locale_string ("bad argument"),
                             SCM_BOOL_F, SCM_BOOL_F);
            }
          scm_foreign_object_set_x (x, 1, NULL);
        }

      // Bad things happen if you allow the stdscr to be garbage
      // collected.
      if (win != stdscr)
        {
          delwin (win);
          scm_foreign_object_set_x (x, 0, NULL);
        }
    }
}

size_t
free_window (SCM x)
{
  assert (SCM_IS_A_P (x, window_fo_type));
  gc_free_window (x);
  return 0;
}

SCM
gucu_is_window_p (SCM x)
{
  return scm_from_bool (_scm_is_window (x));
}

#define u8sym(x) scm_from_utf8_symbol(x)
#define makeFO(a,b,c) scm_make_foreign_object_type((a),(b),(c))
void
gucu_init_type ()
{
  static int first = 1;

  if (first)
    {

      scm_c_define_gsubr ("mevent?", 1, 0, 0, gucu_is_mevent_p);

      screen_fo_type = makeFO (u8sym ("screen"),
                               scm_list_3 (u8sym ("screen"),
                                           u8sym ("ifp"),
                                           u8sym ("ofp")), gc_free_screen);
      scm_c_define_gsubr ("screen?", 1, 0, 0, gucu_is_screen_p);
      scm_c_define_gsubr ("screen=?", 2, 0, 0, gucu_screen_equalp);

      /* The window foreign object has 4 slots.
         1. The ncurses WINDOW * structure for this window, or NULL if
         the window has been freed.
         2. The PANEL * if this window has been turned into a PANEL,
         or NULL otherwise.
         3. The SCM parent <window> of a subwin or derwin, or SCM_BOOL_F if
         there is no parent.
         4. The name of the window for debugging purposes, or SCM_BOOL_F
         if there is no name. */
      window_fo_type = makeFO (u8sym ("window"),
                               scm_list_4 (u8sym ("window"),
                                           u8sym ("panel"),
                                           u8sym ("parent"),
                                           u8sym ("name")), gc_free_window);
      scm_c_define_gsubr ("window?", 1, 0, 0, gucu_is_window_p);
      scm_c_define_gsubr ("window=?", 2, 0, 0, gucu_window_equalp);

      scm_c_define_gsubr ("%scheme-char-to-c-char", 1, 0, 0,
                          gucu_schar_to_char);
      scm_c_define_gsubr ("%scheme-char-to-c-wchar", 1, 0, 0,
                          gucu_schar_to_wchar);
      scm_c_define_gsubr ("%scheme-char-from-c-char", 1, 0, 0,
                          gucu_schar_from_char);
      scm_c_define_gsubr ("%scheme-char-from-c-wchar", 1, 0, 0,
                          gucu_schar_from_wchar);
      scm_c_define_gsubr ("%xchar-from-chtype", 1, 0, 0,
                          gucu_xchar_from_chtype);
      scm_c_define_gsubr ("%xchar-to-chtype", 1, 0, 0, gucu_xchar_to_chtype);

      first = 0;
    }
}
