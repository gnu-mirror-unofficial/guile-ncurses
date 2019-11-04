# Makefile.am for guile-ncurses/test.
#
# Copyright 2009, 2010, 2011, 2014, 2016, 2019 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

TESTLIB = automake-test-lib.scm

AM_TESTS_ENVIRONMENT = \
	ORIGTERM=${TERM}; export ORIGTERM; \
	TERM=xterm; export TERM; \
	GUILE_AUTO_COMPILE=0; export GUILE_AUTO_COMPILE;
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = \
	${top_builddir}/libtool \
	-dlopen ${top_builddir}/src/ncurses/libguile-ncurses.la \
	--mode=execute $(GUILE)
AM_SCM_LOG_FLAGS = -L $(abs_top_srcdir)/src -L $(abs_top_srcdir) -s

NOTERMTESTS = \
 curs_test_setup.scm \
 curs_char_conversion.scm \
 curs_char_wchar_conversion.scm

NARROWTESTS = \
 curs_initscr_endwin.scm \
 curs_initscr_equal.scm \
 curs_initscr_initscr.scm \
 curs_initscr_isendwin_negative_after_refresh.scm \
 curs_initscr_isendwin_negative.scm \
 curs_initscr_isendwin_positive.scm \
 curs_initscr_newterm_screen.scm \
 curs_initscr_newterm_multiple_screens.scm \
 curs_initscr_newterm_stdscr.scm \
 curs_color_assume_default_colors.scm \
 curs_color_attr_get_color.scm \
 curs_color_can_change_color.scm \
 curs_color_color_content_black.scm \
 curs_color_color_content_blue.scm \
 curs_color_color_content_cyan.scm \
 curs_color_color_content_green.scm \
 curs_color_color_content_magenta.scm \
 curs_color_color_content_red.scm \
 curs_color_color_content_white.scm \
 curs_color_color_content_yellow.scm \
 curs_color_color_pairs.scm \
 curs_color_colors.scm \
 curs_color_has_colors.scm \
 curs_color_init_color.scm \
 curs_color_init_pair.scm \
 curs_color_pair_number.scm \
 curs_color_start_color.scm \
 curs_color_use_default_colors.scm \
 curs_addch_acs.scm \
 curs_addch_ascii.scm \
 curs_addchstr.scm \
 curs_addchstr_n.scm \
 curs_addchstr_yx.scm \
 curs_addchstr_yxn.scm \
 curs_addstr.scm \
 curs_addstr_yx.scm \
 curs_addstr_yxn.scm \
 curs_attr_attr_get.scm \
 curs_attr_attr_off_underline.scm \
 curs_attr_attr_on_blink.scm \
 curs_attr_attr_on_bold.scm \
 curs_attr_attr_on_dim.scm \
 curs_attr_attr_on_invis.scm \
 curs_attr_attr_on_protect.scm \
 curs_attr_attr_on_reverse.scm \
 curs_attr_attr_on_standout.scm \
 curs_attr_attr_on_underline.scm \
 curs_attr_attr_set_normal.scm \
 curs_attr_attr_set.scm \
 curs_attr_chgat.scm \
 curs_attr_chgat_yx.scm \
 curs_attr_color_set.scm \
 curs_attr_standend.scm \
 curs_attr_standout.scm \
 curs_beep.scm \
 curs_beep_flash.scm \
 curs_bkgd_bkgd.scm \
 curs_bkgd_bkgdset.scm \
 curs_border_custom.scm \
 curs_border_default.scm \
 curs_border_hline.scm \
 curs_border_hline_yx.scm \
 curs_border_vline.scm \
 curs_border_vline_yx.scm \
 curs_clear.scm \
 curs_clear_clrtobot.scm \
 curs_clear_clrtoeol.scm \
 curs_clear_erase.scm \
 curs_delch_delch.scm \
 curs_delch_delch_yx.scm \
 curs_deleteln_deleteln.scm \
 curs_deleteln_insdelln_negative.scm \
 curs_deleteln_insdelln_positive.scm \
 curs_deleteln_insertln.scm \
 curs_getch_getch.scm \
 curs_getch_getch_yx.scm \
 curs_getch_haskey_invalid.scm \
 curs_getch_haskey.scm \
 curs_getch_key_defined.scm \
 curs_getch_ungetch.scm \
 curs_getstr_getnstr.scm \
 curs_getstr_getnstr_yx.scm \
 curs_getyx_getbegyx.scm \
 curs_getyx_getmaxyx.scm \
 curs_getyx_getparyx.scm \
 curs_getyx_getyx.scm \
 curs_inch.scm \
 curs_inchstr_inchnstr.scm \
 curs_inopts_cbreak.scm \
 curs_inopts_echo.scm \
 curs_inopts_halfdelay.scm \
 curs_inopts_intrflush_false.scm \
 curs_inopts_intrflush.scm \
 curs_inopts_meta_false.scm \
 curs_inopts_meta.scm \
 curs_inopts_nocbreak.scm \
 curs_inopts_noecho.scm \
 curs_inopts_noqiflush.scm \
 curs_inopts_notimeout.scm \
 curs_inopts_qiflush.scm \
 curs_inopts_timeout.scm \
 curs_inopts_typeahead.scm \
 curs_insch_insch.scm \
 curs_insch_insch_yx.scm \
 curs_insstr_insstr.scm \
 curs_insstr_insstr_yx.scm \
 curs_insstr_insstr_n.scm \
 curs_instr_instr.scm \
 curs_instr_instr_n.scm \
 curs_instr_instr_yxn.scm \
 curs_kernel_curs_set.scm \
 curs_kernel_def_prog_mode.scm \
 curs_kernel_def_shell_mode.scm \
 curs_kernel_getsyx.scm \
 curs_kernel_napms.scm \
 curs_kernel_savetty.scm \
 curs_move.scm \
 curs_outopts_clearok.scm \
 curs_outopts_setscrreg.scm \
 curs_overlay_copywin.scm \
 curs_overlay_overlay.scm \
 curs_overlay_overwrite.scm \
 curs_pad_newpad_prefresh.scm \
 curs_pad_pechochar.scm \
 curs_pad_subpad_pnoutrefresh.scm \
 curs_print_mcprint.scm \
 curs_refresh_redrawwin.scm \
 curs_scr_dump.scm \
 curs_scroll_scrl_positive.scm \
 curs_scroll_scrl.scm \
 curs_scroll_scroll.scm \
 curs_termattrs_baudrate.scm \
 curs_termattrs_erasechar.scm \
 curs_termattrs_has_ic.scm \
 curs_termattrs_has_il.scm \
 curs_termattrs_killchar.scm \
 curs_termattrs_longname.scm \
 curs_termattrs_term_attrs.scm \
 curs_termattrs_termname.scm \
 curs_terminfo_setupterm.scm \
 curs_terminfo_setupterm_bad.scm \
 curs_terminfo_tiget_bool.scm \
 curs_terminfo_tiget_num.scm \
 curs_terminfo_tiget_string.scm \
 curs_terminfo_tiget_unknown.scm \
 curs_touch_touchline.scm \
 curs_touch_untouchline.scm \
 curs_touch_untouchwin.scm \
 curs_extend_curses_version.scm \
 curs_extend_use_extended_names.scm \
 curs_util_delay_output.scm \
 curs_util_keyname_1.scm \
 curs_util_keyname_2.scm \
 curs_util_keyname_3.scm \
 curs_util_putwin_getwin.scm \
 curs_util_putwin.scm \
 curs_util_unctrl_1.scm \
 curs_util_unctrl_2.scm \
 curs_util_unctrl_3.scm \
 curs_attributes.scm \
 curs_attributes_strings.scm \
 curs_mouse_getmouse.scm \
 curs_mouse_has_mouse.scm \
 curs_mouse_mouseinterval_default.scm \
 curs_mouse_mouseinterval_set.scm \
 curs_mouse_mousemask.scm \
 curs_mouse_mouse_trafo_from.scm \
 curs_mouse_mouse_trafo_out.scm \
 curs_mouse_mouse_trafo.scm \
 curs_mouse_wenclose_false.scm \
 curs_mouse_wenclose.scm \
 curs_opaque_getdelay.scm \
 curs_opaque_getparent_false.scm \
 curs_opaque_getparent.scm \
 curs_opaque_getscrreg.scm \
 curs_opaque_is_cleared_false.scm \
 curs_opaque_is_cleared.scm \
 curs_opaque_is_idcok_false.scm \
 curs_opaque_is_idcok.scm \
 curs_opaque_is_idlok_false.scm \
 curs_opaque_is_idlok.scm \
 curs_opaque_is_immedok_false.scm \
 curs_opaque_is_immedok.scm \
 curs_opaque_is_keypad_false.scm \
 curs_opaque_is_keypad.scm \
 curs_opaque_is_leaveok_false.scm \
 curs_opaque_is_leaveok.scm \
 curs_opaque_is_nodelay_false.scm \
 curs_opaque_is_nodelay.scm \
 curs_opaque_is_notimeout_false.scm \
 curs_opaque_is_notimeout.scm \
 curs_opaque_is_pad_false.scm \
 curs_opaque_is_pad.scm \
 curs_opaque_is_scrollok_false.scm \
 curs_opaque_is_scrollok.scm \
 curs_opaque_is_subwin_false.scm \
 curs_opaque_is_subwin_pad.scm \
 curs_opaque_is_subwin.scm \
 curs_opaque_is_syncok_false.scm \
 curs_opaque_is_syncok.scm \
 curs_resizeterm_default.scm \
 curs_resizeterm_bad.scm \
 curs_resizeterm_values.scm \
 panel_p_del_panel.scm \
 panel_p_make_panel.scm \
 panel_p_panel.scm \
 panel_p_window.scm \
 panel_update_demo.scm \
 panel_panels_map.scm \
 form_gc_form_alone.scm \
 form_gc_refcount.scm \
 form_new_no_fields.scm \
 form_new_one_field.scm \
 form_data_ahead.scm \
 form_data_behind.scm \
 form_field_attributes_set.scm \
 form_field_attributes_set_back.scm \
 form_field_attributes_set_pad.scm \
 form_field_field_count_one.scm \
 form_field_move_field.scm \
 form_field_set_form_fields.scm \
 form_page_field_index.scm \
 form_page_set_new_page.scm \
 form_field_just.scm \
 form_field_opts_default.scm \
 form_field_opts_set.scm \
 form_field_opts_on.scm \
 form_field_opts_off.scm \
 form_field_buffer_set_field_buffer.scm \
 form_field_buffer_set_field_status.scm \
 form_field_buffer_set_field_status_2.scm \
 menu_equal.scm \
 menu_gc_refcount.scm \
 menu_gc_refcount_2.scm \
 menu_gc_refcount_3.scm \
 menu_gc_menu_alone.scm \
 menu_gc_menu_alone_2.scm \
 menu_gc_menu_still_posted.scm \
 menu_gc_menu_with_win.scm \
 menu_gc_menu_with_win_2.scm \
 menu_new_zero_items.scm \
 menu_new_one_item.scm \
 menu_new_item_in_two_menus.scm \
 mitem_current_item_index.scm \
 mitem_current_set_current_item.scm \
 mitem_current_set_top_row.scm \
 mitem_current_top_row.scm \
 mitem_item_item_name.scm \
 mitem_opts_item_opts_on.scm \
 mitem_opts_item_opts_off.scm \
 mitem_opts_set_item_opts.scm \
 menu_attributes_set.scm \
 menu_attributes_defaults.scm \
 menu_cursor.scm \
 menu_format.scm \
 menu_mark_default.scm \
 menu_mark_set.scm \
 menu_options_default.scm \
 menu_options_set.scm \
 mitem_value_item_value.scm \
 mitem_visible_item_visible.scm \
 slk_323_format.scm \
 slk_44_format.scm \
 slk_444_format.scm \
 slk_attributes_bold.scm

WIDETESTS = \
 curs_border_border_unicode.scm \
 curs_border_hline_unicode.scm \
 curs_border_vline_unicode.scm \
 curs_getch_getch_unicode.scm \
 curs_getstr_getnstr_unicode.scm \
 form_new_unicode.scm

TERMIOSTESTS = \
 termios_type.scm

TESTS = $(NOTERMTESTS)

if HAVE_XTERM_TERMINFO
TESTS += $(NARROWTESTS)
if WIDE
if UCS4
TESTS += $(WIDETESTS)
endif
endif
endif

if TERMIOS
TESTS += $(TERMIOSTESTS)
endif

DEMOS = panel_gc_demo.scm slk_demo.scm termios_demo.scm

EXTRA_DIST = $(TESTLIB) $(NOTERMTESTS) $(NARROWTESTS) $(WIDETESTS) $(TERMIOSTESTS) $(DEMOS)

