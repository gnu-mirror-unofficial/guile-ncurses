/*
  extra_const.c

  Copyright 2010, 2011, 2014, 2016, 2019 Free Software Foundation, Inc.

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
#include <stdlib.h>
#include <fcntl.h>

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

#include "extra_const.h"

SCM gucu_has_termios;
SCM gucu_OCRNL;
SCM gucu_ONLCR;
SCM gucu_ONLRET;
SCM gucu_ONOCR;
SCM gucu_OPOST;
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

SCM gucu_O_RDWR;
SCM gucu_O_NOCTTY;

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

#if HAVE_DECL_OCRNL
  D (OCRNL);
#else
  F (OCRNL);
#endif
#if HAVE_DECL_ONLCR
  D (ONLCR);
#else
  F (ONLCR);
#endif
#if HAVE_DECL_ONLRET
  D (ONLRET);
#else
  F (ONLRET);
#endif
#if HAVE_DECL_ONOCR
  D (ONOCR);
#else
  F (ONOCR);
#endif

  D (ONLCR);
  D (ONLRET);
  D (OPOST);
  D (TCIFLUSH);
  D (TCIOFF);
  D (TCIOFLUSH);
  D (TCION);
  D (TCOFLUSH);
  D (TCOOFF);
  D (TCOON);
  D (TCSADRAIN);
  D (TCSAFLUSH);
  D (TCSANOW);

  D (VEOF);
  D (VEOL);
  D (VERASE);
  D (VINTR);
  D (VKILL);
  D (VMIN);
  D (VQUIT);
  D (VSTART);
  D (VSTOP);
  D (VSUSP);

  D (BRKINT);
  D (ICRNL);
  D (IGNBRK);
  D (IGNCR);
  D (IGNPAR);
  D (INLCR);
  D (INPCK);
  D (ISTRIP);
#if HAVE_DECL_IXANY
  D (IXANY);
#else
  F (IXANY);
#endif
  D (IXOFF);
  D (IXON);
  D (PARMRK);

  D (CLOCAL);
  D (CREAD);
  D (CS5);
  D (CS6);
  D (CS7);
  D (CS8);
  D (CSIZE);
  D (CSTOPB);
  D (HUPCL);
  D (PARENB);
  D (PARODD);
  D (VTIME);

  D (ECHO);
  D (ECHOE);
  D (ECHOK);
  D (ECHONL);
  D (ICANON);
  D (ISIG);
  D (NOFLSH);
  D (TOSTOP);

  D (B0);
  D (B110);
  D (B1200);
  D (B134);
  D (B150);
  D (B1800);
  D (B19200);
  D (B200);
  D (B2400);
  D (B300);
  D (B38400);
  D (B4800);
  D (B50);
  D (B600);
  D (B75);
  D (B9600);
#if HAVE_DECL_B115200
  D (B115200);
#else
  F (B115200);
#endif
#if HAVE_DECL_B14400
  D (B14400);
#else
  F (B14400);
#endif
#if HAVE_DECL_B230400
  D (B230400);
#else
  F (B230400);
#endif
#if HAVE_DECL_B28800
  D (B28800);
#else
  F (B28800);
#endif
#if HAVE_DECL_B57600
  D (B57600);
#else
  F (B57600);
#endif
#if HAVE_DECL_B7200
  D (B7200);
#else
  F (B7200);
#endif
#if HAVE_DECL_B76800
  D (B76800);
#else
  F (B76800);
#endif
#if HAVE_DECL_O_RDWR
  D (O_RDWR);
#else
  F (O_RDWR);
#endif
#if HAVE_DECL_O_NOCTTY
  D (O_NOCTTY);
#else
  F (O_NOCTTY);
#endif
}
