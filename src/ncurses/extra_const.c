/*
  extra_const.c

  Copyright 2010, 2011, 2014, 2016 Free Software Foundation, Inc.

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

#include <libguile.h>
#if HAVE_TERMIOS_H
#include <termios.h>
#endif

#if HAVE_CURSES_H
#include <curses.h>
#include <form.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#include <ncurses/form.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/curses.h>
#include <ncursesw/form.h>
#else
#error "No curses.h file included"
#endif

#include "compat.h"
#include "extra_const.h"

SCM gucu_has_termios;
SCM gucu_BS0;
SCM gucu_BS1;
SCM gucu_BSDLY;
SCM gucu_CR0;
SCM gucu_CR1;
SCM gucu_CR2;
SCM gucu_CR3;
SCM gucu_CRDLY;
SCM gucu_FF0;
SCM gucu_FF1;
SCM gucu_FFDLY;
SCM gucu_NL0;
SCM gucu_NL1;
SCM gucu_NLDLY;
SCM gucu_OCRNL;
SCM gucu_OFILL;
SCM gucu_ONLCR;
SCM gucu_ONLRET;
SCM gucu_ONOCR;
SCM gucu_OPOST;
SCM gucu_TAB0;
SCM gucu_TAB1;
SCM gucu_TAB2;
SCM gucu_TAB3;
SCM gucu_TABDLY;
SCM gucu_TCIFLUSH;
SCM gucu_TCIOFF;
SCM gucu_TCIOFLUSH;
SCM gucu_TCION;
SCM gucu_TCOFLUSH;
SCM gucu_TCOOFF;
SCM gucu_TCOON;
SCM gucu_TCSADRAIN;
SCM gucu_TCSAFLUSH;
SCM gucu_TCSANOW;
SCM gucu_VT0;
SCM gucu_VT1;
SCM gucu_VTDLY;

SCM gucu_VEOF;
SCM gucu_VEOL;
SCM gucu_VERASE;
SCM gucu_VINTR;
SCM gucu_VKILL;
SCM gucu_VMIN;
SCM gucu_VQUIT;
SCM gucu_VSTART;
SCM gucu_VSTOP;
SCM gucu_VSUSP;

SCM gucu_BRKINT;
SCM gucu_ICRNL;
SCM gucu_IGNBRK;
SCM gucu_IGNCR;
SCM gucu_IGNPAR;
SCM gucu_INLCR;
SCM gucu_INPCK;
SCM gucu_ISTRIP;
SCM gucu_IXANY;
SCM gucu_IXOFF;
SCM gucu_IXON;
SCM gucu_PARMRK;

SCM gucu_CLOCAL;
SCM gucu_CREAD;
SCM gucu_CS5;
SCM gucu_CS6;
SCM gucu_CS7;
SCM gucu_CS8;
SCM gucu_CSIZE;
SCM gucu_CSTOPB;
SCM gucu_HUPCL;
SCM gucu_PARENB;
SCM gucu_PARODD;
SCM gucu_VTIME;

SCM gucu_ECHO;
SCM gucu_ECHOE;
SCM gucu_ECHOK;
SCM gucu_ECHONL;
SCM gucu_ICANON;
SCM gucu_IEXTEN;
SCM gucu_ISIG;
SCM gucu_NOFLSH;
SCM gucu_TOSTOP;

SCM gucu_B0;
SCM gucu_B50;
SCM gucu_B75;
SCM gucu_B110;
SCM gucu_B134;
SCM gucu_B150;
SCM gucu_B200;
SCM gucu_B300;
SCM gucu_B600;
SCM gucu_B1200;
SCM gucu_B1800;
SCM gucu_B2400;
SCM gucu_B4800;
SCM gucu_B9600;
SCM gucu_B19200;
SCM gucu_B38400;

SCM gucu_B115200;
SCM gucu_B14400;
SCM gucu_B230400;
SCM gucu_B28800;
SCM gucu_B57600;
SCM gucu_B7200;
SCM gucu_B76800;


#ifdef ENABLE_TERMIOS
#define D(x) gucu_ ## x = scm_permanent_object(scm_c_define(#x, scm_from_int(x)))
#else
#define D(x) gucu_ ## x = scm_permanent_object(scm_c_define(#x, SCM_BOOL_F))
#endif

#define F(x) gucu_ ## x = scm_permanent_object(scm_c_define(#x, SCM_BOOL_F))

void
gucu_extra_init_const ()
{
#ifdef ENABLE_TERMIOS
  gucu_has_termios =
    scm_permanent_object (scm_c_define ("%has-termios", SCM_BOOL_T));
#else
  gucu_has_termios =
    scm_permanent_object (scm_c_define ("%has-termios", SCM_BOOL_F));
#endif

#if HAVE_DECL_BS0
  D(BS0);
#else
  F(BS0);
#endif
#if HAVE_DECL_BS1
  D(BS1);
#else
  F(BS1);
#endif
#if HAVE_DECL_BSDLY
  D(BSDLY);
#else
  F(BSDLY);
#endif
#if HAVE_DECL_CR0
  D(CR0);
#else
  F(CR0);
#endif
#if HAVE_DECL_CR1
  D(CR1);
#else
  F(CR1);
#endif
#if HAVE_DECL_CR2
  D(CR2);
#else
  F(CR2);
#endif
#if HAVE_DECL_CR3
  D(CR3);
#else
  F(CR3);
#endif
#if HAVE_DECL_CRDLY
  D(CRDLY);
#else
  F(CRDLY);
#endif
#if HAVE_DECL_FF0
  D(FF0);
#else
  F(FF0);
#endif
#if HAVE_DECL_FF1
  D(FF1);
#else
  F(FF1);
#endif
#if HAVE_DECL_FFDLY
  D(FFDLY);
#else
  F(FFDLY);
#endif
#if HAVE_DECL_NL0
  D(NL0);
#else
  F(NL0);
#endif
#if HAVE_DECL_NL1
  D(NL1);
#else
  F(NL1);
#endif
#if HAVE_DECL_NLDLY
  D(NLDLY);
#else
  F(NLDLY);
#endif
#if HAVE_DECL_OCRNL
  D(OCRNL);
#else
  F(OCRNL);
#endif
#if HAVE_DECL_OFILL
  D(OFILL);
#else
  F(OFILL);
#endif
#if HAVE_DECL_ONLCR
  D(ONLCR);
#else
  F(ONLCR);
#endif
#if HAVE_DECL_ONLRET
  D(ONLRET);
#else
  F(ONLRET);
#endif
#if HAVE_DECL_ONOCR
  D(ONOCR);
#else
  F(ONOCR);
#endif
#if HAVE_DECL_TAB0
  D(TAB0);
#else
  F(TAB0);
#endif
#if HAVE_DECL_TAB1
  D(TAB1);
#else
  F(TAB1);
#endif
#if HAVE_DECL_TAB2
  D(TAB2);
#else
  F(TAB2);
#endif
#if HAVE_DECL_TAB3
  D(TAB3);
#else
  F(TAB3);
#endif
#if HAVE_DECL_TABDLY
  D(TABDLY);
#else
  F(TABDLY);
#endif
#if HAVE_DECL_VT0
  D(VT0);
#else
  F(VT0);
#endif
#if HAVE_DECL_VT1
  D(VT1);
#else
  F(VT1);
#endif
#if HAVE_DECL_VTDLY
  D(VTDLY);
#else
  F(VTDLY);
#endif

  D(ONLCR);
  D(ONLRET);
  D(OPOST);
  D(TCIFLUSH);
  D(TCIOFF);
  D(TCIOFLUSH);
  D(TCION);
  D(TCOFLUSH);
  D(TCOOFF);
  D(TCOON);
  D(TCSADRAIN);
  D(TCSAFLUSH);
  D(TCSANOW);
  D(VT0);
  D(VT1);
  D(VTDLY);

  D(VEOF);
  D(VEOL);
  D(VERASE);
  D(VINTR);
  D(VKILL);
  D(VMIN);
  D(VQUIT);
  D(VSTART);
  D(VSTOP);
  D(VSUSP);

  D(BRKINT);
  D(ICRNL);
  D(IGNBRK);
  D(IGNCR);
  D(IGNPAR);
  D(INLCR);
  D(INPCK);
  D(ISTRIP);
  #if HAVE_DECL_IXANY
  D(IXANY);
  #else
  F(IXANY);
  #endif
  D(IXOFF);
  D(IXON);
  D(PARMRK);

  D(CLOCAL);
  D(CREAD);
  D(CS5);
  D(CS6);
  D(CS7);
  D(CS8);
  D(CSIZE);
  D(CSTOPB);
  D(HUPCL);
  D(PARENB);
  D(PARODD);
  D(VTIME);

  D(ECHO);
  D(ECHOE);
  D(ECHOK);
  D(ECHONL);
  D(ICANON);
  D(IEXTEN);
  D(ISIG);
  D(NOFLSH);
  D(TOSTOP);

  D(B0);
  D(B110);
  D(B1200);
  D(B134);
  D(B150);
  D(B1800);
  D(B19200);
  D(B200);
  D(B2400);
  D(B300);
  D(B38400);
  D(B4800);
  D(B50);
  D(B600);
  D(B75);
  D(B9600);
#if HAVE_DECL_B115200
  D(B115200);
#else
  F(B115200);
#endif
#if HAVE_DECL_B14400
  D(B14400);
#else
  F(B14400);
#endif
#if HAVE_DECL_B230400
  D(B230400);
#else
  F(B230400);
#endif
#if HAVE_DECL_B28800
  D(B28800);
#else
  F(B28800);
#endif
#if HAVE_DECL_B57600
  D(B57600);
#else
  F(B57600);
#endif
#if HAVE_DECL_B7200
  D(B7200);
#else
  F(B7200);
#endif
#if HAVE_DECL_B76800
  D(B76800);
#else
  F(B76800);
#endif
}
