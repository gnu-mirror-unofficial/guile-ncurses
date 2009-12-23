#include <libguile.h>
#include <curses.h>
#include <config.h>

#include "slk_func.h"
#include "type.h"
#include "compat.h"

#define RETURNTF(x) \
  if(x==ERR) \
    return SCM_BOOL_F; \
  else \
    return SCM_BOOL_T

SCM
gucu_slk_attr_off (SCM attrs)
{
  SCM_ASSERT (_scm_is_attr (attrs), attrs, SCM_ARG1, "slk-attr-off");
  
  const attr_t c_attrs = _scm_to_attr (attrs);

  int ret = slk_attr_off (c_attrs, NULL);
  RETURNTF (ret);
}

SCM
gucu_slk_attroff (SCM attrs)
{
  SCM_ASSERT (_scm_is_chtype (attrs), attrs, SCM_ARG1, "slk-attroff");
  
  const chtype c_attrs = _scm_to_chtype (attrs);
  
  int ret = slk_attroff (c_attrs);
  RETURNTF(ret);
}

SCM
gucu_slk_attr_on (SCM attrs)
{
  SCM_ASSERT (_scm_is_attr (attrs), attrs, SCM_ARG1, "slk-attr-on");

  const attr_t c_attrs = _scm_to_attr (attrs);

  int ret = slk_attr_on (c_attrs, NULL);
  RETURNTF(ret);
}

SCM
gucu_slk_attron (SCM attrs)
{
  SCM_ASSERT (_scm_is_chtype (attrs), attrs, SCM_ARG1, "slk-attron");

  const chtype c_attrs = _scm_to_chtype (attrs);
  
  int ret = slk_attron (c_attrs);
  RETURNTF(ret);
}

SCM
gucu_slk_attr_set (SCM attrs, SCM color_pair_number)
{
  SCM_ASSERT (_scm_is_attr (attrs), attrs, SCM_ARG1, "slk-attr-set");
  SCM_ASSERT (scm_is_integer (color_pair_number), color_pair_number, SCM_ARG2, 
	      "slk-attr-set");

  const attr_t c_attrs = _scm_to_attr (attrs);
  short c_color_pair_number = scm_to_short (color_pair_number);

  int ret = slk_attr_set (c_attrs, c_color_pair_number, NULL);
  RETURNTF(ret);
}

SCM
gucu_slk_attrset (SCM attrs)
{
  SCM_ASSERT (_scm_is_chtype (attrs), attrs, SCM_ARG1, "slk-attrset");

  const chtype c_attrs = _scm_to_chtype (attrs);
  
  int ret = slk_attrset (c_attrs);
  RETURNTF(ret);
}

SCM
gucu_slk_clear ()
{
  int ret = slk_clear ();
  RETURNTF(ret);
}

SCM
gucu_slk_color (SCM color_pair_number)
{
  SCM_ASSERT (scm_is_integer (color_pair_number), color_pair_number, SCM_ARG1, 
	      "slk-color");

  short c_color_pair_number = scm_to_short (color_pair_number);

  int ret = slk_color (c_color_pair_number);
  RETURNTF(ret);
}

SCM
gucu_slk_init (SCM fmt)
{
  SCM_ASSERT (scm_is_integer (fmt), fmt, SCM_ARG1, "slk-init");
  
  int c_fmt = scm_to_int (fmt);
  
  int ret = slk_init (c_fmt);
  RETURNTF(ret);
}

SCM
gucu_slk_label (SCM labnum)
{
  SCM_ASSERT (scm_is_integer (labnum), labnum, SCM_ARG1, "slk-label");
  
  int c_labnum = scm_to_int (labnum);
  
  char *ret = slk_label (c_labnum);
  SCM s_ret = scm_from_locale_string (ret);
  
  return s_ret;
}

SCM
gucu_slk_noutrefresh ()
{
  int ret = slk_noutrefresh ();
  RETURNTF(ret);
}

SCM
gucu_slk_refresh ()
{
  int ret = slk_refresh ();
  RETURNTF(ret);
}

SCM
gucu_slk_restore ()
{
  int ret = slk_restore ();
  RETURNTF(ret);
}

SCM
gucu_slk_set (SCM labnum, SCM label, SCM fmt)
{
  SCM_ASSERT (scm_is_integer (labnum), labnum, SCM_ARG1, "slk-set");
  SCM_ASSERT (scm_is_string (label), label, SCM_ARG2, "slk-set");
  SCM_ASSERT (scm_is_integer (fmt), fmt, SCM_ARG3, "slk-set");
  
  int c_labnum = scm_to_int (labnum);
  char *c_label = scm_to_locale_string (label);
  int c_fmt = scm_to_int (fmt);
  
  int ret = slk_set (c_labnum, c_label, c_fmt);
  RETURNTF(ret);
}

SCM
gucu_slk_touch ()
{
  int ret = slk_touch ();
  RETURNTF(ret);
}


void 
gucu_slk_init_function ()
{
  static int first = 1;
  if (first)
    {
      scm_c_define_gsubr ("slk-attr-off", 1, 0, 0, gucu_slk_attr_off);
      scm_c_define_gsubr ("slk-attroff", 1, 0, 0, gucu_slk_attroff);
      scm_c_define_gsubr ("slk-attr-on", 1, 0, 0, gucu_slk_attr_on);
      scm_c_define_gsubr ("slk-attron", 1, 0, 0, gucu_slk_attron);
      scm_c_define_gsubr ("slk-attr-set", 2, 0, 0, gucu_slk_attr_set);
      scm_c_define_gsubr ("slk-attrset", 1, 0, 0, gucu_slk_attrset);
      scm_c_define_gsubr ("slk-clear", 0, 0, 0, gucu_slk_clear);
      scm_c_define_gsubr ("slk-color", 1, 0, 0, gucu_slk_color);
      scm_c_define_gsubr ("slk-init", 1, 0, 0, gucu_slk_init);
      scm_c_define_gsubr ("slk-label", 1, 0, 0, gucu_slk_label);
      scm_c_define_gsubr ("slk-noutrefresh", 0, 0, 0, gucu_slk_noutrefresh);
      scm_c_define_gsubr ("slk-refresh", 0, 0, 0, gucu_slk_refresh);
      scm_c_define_gsubr ("slk-restore", 0, 0, 0, gucu_slk_restore);
      scm_c_define_gsubr ("slk-set", 3, 0, 0, gucu_slk_set);
      scm_c_define_gsubr ("slk-touch", 0, 0, 0, gucu_slk_touch);
      first = 0;
    }
}

