/*
  extra_func.c

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
#include <gucuconfig.h>

#include <assert.h>
#include <libguile.h>
#include <sys/types.h>
#ifdef GUILE_CHARS_ARE_UCS4
#include <uniwidth.h>
#include <unilbrk.h>
#else
#include <wchar.h>
#endif
#ifdef ENABLE_TERMIOS
#include <termios.h>
#endif

#include "extra_func.h"
#include "extra_type.h"
#include "type.h"

#ifdef ENABLE_TERMIOS
SCM
gucu_cfgetispeed (SCM s_termios)
{
  struct termios *c_termios;
  speed_t c_ret;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "cfgetispeed");
  c_termios = _scm_to_termios (s_termios);
  c_ret = cfgetispeed (c_termios);
  return scm_from_uint (c_ret);
}

SCM
gucu_cfgetospeed (SCM s_termios)
{
  struct termios *c_termios;
  speed_t c_ret;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "cfgetospeed");
  c_termios = _scm_to_termios (s_termios);
  c_ret = cfgetospeed (c_termios);
  return scm_from_uint (c_ret);
}

SCM
gucu_cfmakeraw_x (SCM s_termios)
{
  struct termios *c_termios;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1, "cfmakeraw!");
  c_termios = _scm_to_termios (s_termios);
  cfmakeraw (c_termios);
  return SCM_UNSPECIFIED;
}

SCM
gucu_cfsetispeed_x (SCM s_termios, SCM s_speed)
{
  struct termios *c_termios;
  speed_t c_speed;
  int c_ret;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "cfsetispeed!");
  SCM_ASSERT (scm_is_integer (s_speed), s_speed, SCM_ARG2, "cfsetispeed!");
  c_termios = _scm_to_termios (s_termios);
  c_speed = scm_to_uint (s_speed);
  c_ret = cfsetispeed (c_termios, c_speed);
  if (c_ret == 0)
    scm_syserror ("cfsetispeed!");
  return SCM_UNSPECIFIED;
}

SCM
gucu_cfsetospeed_x (SCM s_termios, SCM s_speed)
{
  struct termios *c_termios;
  speed_t c_speed;
  int c_ret;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "cfsetospeed!");
  SCM_ASSERT (scm_is_integer (s_speed), s_speed, SCM_ARG2, "cfsetospeed!");
  c_termios = _scm_to_termios (s_termios);
  c_speed = scm_to_uint (s_speed);
  c_ret = cfsetospeed (c_termios, c_speed);
  if (c_ret != 0)
    scm_syserror ("cfsetospeed!");
  return SCM_UNSPECIFIED;
}

#ifdef HAVE_POSIX_OPENPT
/* Returns the file descriptor of a new master pseudo-terminal. */
SCM
gucu_openpt (SCM s_flags)
{
  int flags;
  int fd;

  SCM_ASSERT (scm_is_integer (s_flags), s_flags, SCM_ARG1, "openpt");
  flags = scm_to_int (s_flags);
  fd = posix_openpt (flags);
  if (fd == -1)
    scm_syserror ("openpt");
  return scm_fdopen (scm_from_int (fd), scm_from_latin1_string ("rw0"));
}
#endif

#ifdef HAVE_GRANTPT
/* If FD is the file descriptor of a master pseudo-terminal, this
   changes the mode and permissions of the slave pseudo-terminal
   so that it can be used.  */
SCM
gucu_grantpt (SCM s_fd_or_port)
{
  int c_fd;
  int ret;
  SCM s_fd;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("grantpt", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);
  ret = grantpt (c_fd);
  if (ret == -1)
    scm_syserror ("grantpt");

  return SCM_UNSPECIFIED;
}
#endif


/* IF FD is a file descriptor of a pseudo-terminal device,
   this sets that pseudoterminal to RAW mode. */
SCM
gucu_ptsmakeraw (SCM s_fd_or_port)
{
  struct termios terminal_attributes;
  SCM s_fd;
  int c_fd;
  int ret;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("ptsmakeraw", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);

  ret = tcgetattr (c_fd, &terminal_attributes);
  if (ret == -1)
    scm_syserror ("ptsmakeraw");
  terminal_attributes.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP
                                   | INLCR | IGNCR | ICRNL | IXON);
  terminal_attributes.c_oflag &= ~OPOST;
  terminal_attributes.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
  terminal_attributes.c_cflag &= ~(CSIZE | PARENB);
  terminal_attributes.c_cflag |= CS8;
  ret = tcsetattr (c_fd, TCSANOW, &terminal_attributes);
  if (ret == -1)
    scm_syserror ("ptsmakeraw");
  return SCM_UNDEFINED;
}

#ifdef HAVE_PTSNAME
/* If FD, a file descriptor, is a master pseudo-terminal device, this
   returns a string that contains the name of the slave
   pseudo-terminal device.  */
SCM
gucu_ptsname (SCM s_fd_or_port)
{
  SCM s_fd;
  int c_fd;
  char *name;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("ptsname", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);
  name = ptsname (c_fd);
  if (name == NULL)
    scm_syserror ("ptsname");

  return scm_from_locale_string (name);
}
#endif

SCM
gucu_tcdrain (SCM s_fd_or_port)
{
  SCM s_fd;
  int c_fd, c_ret;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("tcdrain", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);

  c_ret = tcdrain (c_fd);

  if (c_ret != 0)
    scm_syserror ("tcdrain");
  return SCM_UNSPECIFIED;
}

SCM
gucu_tcflow (SCM s_fd_or_port, SCM s_action)
{
  SCM s_fd;
  int c_fd, c_action, c_ret;

  SCM_ASSERT (scm_is_integer (s_action), s_action, SCM_ARG2, "tcflow");

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("tcflow", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);
  c_action = scm_to_int (s_action);
  c_ret = tcflow (c_fd, c_action);

  if (c_ret != 0)
    scm_syserror ("tcflow");
  return SCM_UNSPECIFIED;
}

SCM
gucu_tcflush (SCM s_fd_or_port, SCM s_queue)
{
  SCM s_fd;
  int c_fd, c_queue, c_ret;

  SCM_ASSERT (scm_is_integer (s_queue), s_queue, SCM_ARG2, "tcflush");

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("tcflush", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);
  c_queue = scm_to_int (s_queue);
  c_ret = tcflush (c_fd, c_queue);

  if (c_ret != 0)
    scm_syserror ("tcflush");
  return SCM_UNSPECIFIED;
}

SCM
gucu_tcgetattr (SCM s_fd_or_port)
{
  SCM s_fd, s_termios;
  struct termios *c_termios;
  int c_fd, c_ret;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("tcgetattr", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);
  s_termios = gucu_new_termios ();
  c_termios = _scm_to_termios (s_termios);
  c_ret = tcgetattr (c_fd, c_termios);

  if (c_ret != 0)
    scm_syserror ("tcgetattr");

  return s_termios;
}

#ifdef HAVE_TCGETSID
SCM
gucu_tcgetsid (SCM s_fd_or_port)
{
  SCM s_fd;
  int c_fd;
  pid_t c_pid;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("tcgetsid", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);

  c_pid = tcgetsid (c_fd);

  if (c_pid == (pid_t) - 1)
    scm_syserror ("tcgetsid");
  return scm_from_int (c_pid);
}
#endif


SCM
gucu_tcsendbreak (SCM s_fd_or_port, SCM s_duration)
{
  SCM s_fd;
  int c_fd, c_duration, c_ret;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("tcsendbreak", SCM_ARG1, s_fd_or_port);

  SCM_ASSERT (scm_is_integer (s_duration), s_duration, SCM_ARG2,
              "tcsendbreak");

  c_fd = scm_to_int (s_fd);
  c_duration = scm_to_int (s_duration);
  c_ret = tcsendbreak (c_fd, c_duration);

  if (c_ret != 0)
    scm_syserror ("tcsendbreak");
  return SCM_UNSPECIFIED;
}


SCM
gucu_tcsetattr_x (SCM s_fd_or_port, SCM s_opt, SCM s_termios)
{
  SCM s_fd;
  int c_fd, c_opt, c_ret;
  struct termios *c_termios;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("tcsetattr!", SCM_ARG1, s_fd_or_port);
  SCM_ASSERT (scm_is_integer (s_opt), s_opt, SCM_ARG2, "tcsetattr!");
  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG3, "tcsetattr!");

  c_fd = scm_to_int (s_fd);
  c_opt = scm_to_int (s_opt);
  c_termios = _scm_to_termios (s_termios);
  c_ret = tcsetattr (c_fd, c_opt, c_termios);
  if (c_ret != 0)
    scm_syserror ("tcsetattr!");
  return SCM_UNSPECIFIED;
}

SCM
gucu_termios_iflag (SCM s_termios)
{
  struct termios *c_termios;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-iflag");
  c_termios = _scm_to_termios (s_termios);
  return scm_from_uint (c_termios->c_iflag);
}

SCM
gucu_termios_oflag (SCM s_termios)
{
  struct termios *c_termios;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-oflag");
  c_termios = _scm_to_termios (s_termios);
  return scm_from_uint (c_termios->c_oflag);
}

SCM
gucu_termios_cflag (SCM s_termios)
{
  struct termios *c_termios;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-cflag");
  c_termios = _scm_to_termios (s_termios);
  return scm_from_uint (c_termios->c_cflag);
}

SCM
gucu_termios_lflag (SCM s_termios)
{
  struct termios *c_termios;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-lflag");
  c_termios = _scm_to_termios (s_termios);
  return scm_from_uint (c_termios->c_lflag);
}

SCM
gucu_termios_cc (SCM s_termios, SCM s_mode)
{
  int c_mode;
  struct termios *c_termios;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1, "termios-cc");
  SCM_ASSERT (scm_is_integer (s_mode), s_mode, SCM_ARG2, "termios-cc");

  c_termios = _scm_to_termios (s_termios);
  c_mode = scm_to_int (s_mode);
  if (c_mode < 0 || c_mode >= NCCS)
    scm_out_of_range ("termios-cc", s_mode);

  return scm_integer_to_char (scm_from_uchar (c_termios->c_cc[c_mode]));
}

SCM
gucu_termios_iflag_set_x (SCM s_termios, SCM s_flag)
{
  struct termios *c_termios;
  unsigned int c_flag;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-iflag-set!");
  c_termios = _scm_to_termios (s_termios);
  c_flag = scm_to_uint (s_flag);
  c_termios->c_iflag = c_flag;
  return SCM_UNSPECIFIED;
}

SCM
gucu_termios_oflag_set_x (SCM s_termios, SCM s_flag)
{
  struct termios *c_termios;
  unsigned int c_flag;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-oflag-set!");
  c_termios = _scm_to_termios (s_termios);
  c_flag = scm_to_uint (s_flag);
  c_termios->c_oflag = c_flag;
  return SCM_UNSPECIFIED;
}

SCM
gucu_termios_cflag_set_x (SCM s_termios, SCM s_flag)
{
  struct termios *c_termios;
  unsigned int c_flag;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-cflag-set!");
  c_termios = _scm_to_termios (s_termios);
  c_flag = scm_to_uint (s_flag);
  c_termios->c_cflag = c_flag;
  return SCM_UNSPECIFIED;
}

SCM
gucu_termios_lflag_set_x (SCM s_termios, SCM s_flag)
{
  struct termios *c_termios;
  unsigned int c_flag;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-lflag-set!");
  c_termios = _scm_to_termios (s_termios);
  c_flag = scm_to_uint (s_flag);
  c_termios->c_lflag = c_flag;
  return SCM_UNSPECIFIED;
}

SCM
gucu_termios_cc_set_x (SCM s_termios, SCM s_pos, SCM s_char)
{
  struct termios *c_termios;
  int c_pos;
  int c_char;

  SCM_ASSERT (_scm_is_termios (s_termios), s_termios, SCM_ARG1,
              "termios-cc-set!");
  SCM_ASSERT (scm_is_integer (s_pos), s_pos, SCM_ARG2, "termios-cc-set!");
  SCM_ASSERT (SCM_CHARP (s_char), s_char, SCM_ARG3, "termios-cc-set!");

  c_termios = _scm_to_termios (s_termios);
  c_pos = scm_to_int (s_pos);
  if (c_pos < 0 || c_pos >= NCCS)
    scm_out_of_range ("termios-cc-set!", s_pos);
  c_char = (int) SCM_CHAR (s_char);
  if (c_char < 0 || c_char > 255)
    scm_out_of_range ("termios-cc-set!", s_char);

  c_termios->c_cc[c_pos] = c_char;
  return SCM_UNSPECIFIED;
}

#ifdef HAVE_UNLOCKPT
/* If FD is the file descriptor of a master pseudo-terminal, this
   changes the mode and permissions of the slave pseudo-terminal
   so that it can be used.  */
SCM
gucu_unlockpt (SCM s_fd_or_port)
{
  SCM s_fd;
  int c_fd;
  int ret;

  if (scm_is_true (scm_port_p (s_fd_or_port)))
    s_fd = scm_fileno (s_fd_or_port);
  else if (scm_is_integer (s_fd_or_port))
    s_fd = s_fd_or_port;
  else
    scm_wrong_type_arg ("unlockpt", SCM_ARG1, s_fd_or_port);

  c_fd = scm_to_int (s_fd);
  ret = unlockpt (c_fd);
  if (ret == -1)
    scm_syserror ("unlockpt");

  return SCM_UNSPECIFIED;
}
#endif

#endif /* ENABLE_TERMIOS */

/* Return the number of character cells that string requires */
SCM
gucu_strwidth (SCM str)
{
#ifdef GUILE_CHARS_ARE_UCS4
  SCM s_siz;
  size_t i, len;
  uint32_t *s;

  SCM_ASSERT (scm_is_string (str), str, SCM_ARG1, "%strwidth");
  len = scm_c_string_length (str);
  s = (uint32_t *) malloc ((len + 1) * sizeof (uint32_t));
  for (i = 0; i < len; i++)
    s[i] = SCM_CHAR (scm_c_string_ref (str, i));
  s[len] = 0;
  s_siz = scm_from_int (u32_strwidth (s, "UTF-8"));
  free (s);
  return s_siz;
#else
#ifdef HAVE_WCWIDTH
  size_t i, len, s, siz;

  SCM_ASSERT (scm_is_string (str), str, SCM_ARG1, "%strwidth");
  len = scm_c_string_length (str);
  siz = 0;
  for (i = 0; i < len; i++)
    {
      s = wcwidth (btowc (SCM_CHAR (scm_c_string_ref (str, i))));
      if (s >= 0 && s <= 2)
        siz += s;
    }
  return scm_from_int (siz);
#else
  return scm_c_string_length (str);
#endif
#endif
}

SCM
gucu_string_split_at_line_endings (SCM str)
{
  SCM res = SCM_EOL;

  SCM_ASSERT (scm_is_string (str), str, SCM_ARG1,
              "string-split-at-line-endings");

  long idx, last_idx;

  idx = scm_c_string_length (str);
  while (idx >= 0)
    {
      last_idx = idx;

      do
        {
          int c =
            scm_to_int (scm_char_to_integer
                        (scm_c_string_ref (str, idx - 1)));
          if ((c == 0xD) | (c == 0xA) || (c == 0x85) || (c == 0x2028)
              || (c == 2029))
            break;
          idx--;
        }
      while (idx > 0);
      if ((idx >= 2)
          &&
          (scm_to_int (scm_char_to_integer (scm_c_string_ref (str, idx - 1)))
           == 0xA)
          &&
          (scm_to_int (scm_char_to_integer (scm_c_string_ref (str, idx - 2)))
           == 0xD))
        {
          res = scm_cons (scm_c_substring_copy (str, idx, last_idx), res);
          idx -= 2;
        }
      else if (idx >= 0)
        {
          res = scm_cons (scm_c_substring_copy (str, idx, last_idx), res);
          idx--;
        }
    }

  scm_remember_upto_here_1 (str);
  return res;
}

void
gucu_extra_init_function ()
{
#ifdef ENABLE_TERMIOS
  scm_c_define_gsubr ("cfgetispeed", 1, 0, 0, gucu_cfgetispeed);
  scm_c_define_gsubr ("cfgetospeed", 1, 0, 0, gucu_cfgetospeed);
  scm_c_define_gsubr ("cfmakeraw!", 1, 0, 0, gucu_cfmakeraw_x);
  scm_c_define_gsubr ("cfsetispeed!", 2, 0, 0, gucu_cfsetispeed_x);
  scm_c_define_gsubr ("cfsetospeed!", 2, 0, 0, gucu_cfsetospeed_x);
#ifdef HAVE_POSIX_OPENPT
  scm_c_define_gsubr ("openpt", 1, 0, 0, gucu_openpt);
#endif
#ifdef HAVE_GRANTPT
  scm_c_define_gsubr ("grantpt", 1, 0, 0, gucu_grantpt);
#endif
  scm_c_define_gsubr ("ptsmakeraw", 1, 0, 0, gucu_ptsmakeraw);
#ifdef HAVE_PTSNAME
  scm_c_define_gsubr ("ptsname", 1, 0, 0, gucu_ptsname);
#endif
  scm_c_define_gsubr ("tcdrain", 1, 0, 0, gucu_tcdrain);
  scm_c_define_gsubr ("tcflow", 2, 0, 0, gucu_tcflow);
  scm_c_define_gsubr ("tcflush", 2, 0, 0, gucu_tcflush);
  scm_c_define_gsubr ("tcgetattr", 1, 0, 0, gucu_tcgetattr);
#ifdef HAVE_TCGETSID
  scm_c_define_gsubr ("tcgetsid", 1, 0, 0, gucu_tcgetsid);
#endif
  scm_c_define_gsubr ("tcsendbreak", 2, 0, 0, gucu_tcsendbreak);
  scm_c_define_gsubr ("tcsetattr!", 3, 0, 0, gucu_tcsetattr_x);
#ifdef HAVE_UNLOCKPT
  scm_c_define_gsubr ("unlockpt", 1, 0, 0, gucu_unlockpt);
#endif

  scm_c_define_gsubr ("termios-iflag", 1, 0, 0, gucu_termios_iflag);
  scm_c_define_gsubr ("termios-oflag", 1, 0, 0, gucu_termios_oflag);
  scm_c_define_gsubr ("termios-cflag", 1, 0, 0, gucu_termios_cflag);
  scm_c_define_gsubr ("termios-lflag", 1, 0, 0, gucu_termios_lflag);
  scm_c_define_gsubr ("termios-cc", 2, 0, 0, gucu_termios_cc);
  scm_c_define_gsubr ("termios-iflag-set!", 2, 0, 0,
                      gucu_termios_iflag_set_x);
  scm_c_define_gsubr ("termios-oflag-set!", 2, 0, 0,
                      gucu_termios_oflag_set_x);
  scm_c_define_gsubr ("termios-cflag-set!", 2, 0, 0,
                      gucu_termios_cflag_set_x);
  scm_c_define_gsubr ("termios-lflag-set!", 2, 0, 0,
                      gucu_termios_lflag_set_x);
  scm_c_define_gsubr ("termios-cc-set!", 3, 0, 0, gucu_termios_cc_set_x);
#endif /* ENABLE_TERMIOS */

#if defined(GUILE_CHARS_ARE_UCS4) || defined(HAVE_WCWIDTH)
  scm_c_define_gsubr ("%strwidth", 1, 0, 0, gucu_strwidth);
#endif

#ifdef GUILE_CHARS_ARE_UCS4
  scm_c_define_gsubr ("string-split-at-line-endings", 1, 0, 0,
                      gucu_string_split_at_line_endings);
#endif
}
