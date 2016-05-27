;; Copyright 2009, 2010, 2016 Free Software Foundation, Inc.

;; This file is part of Guile-Ncurses.

;; Guile-Ncurses is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Guile-Ncurses is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Guile-Ncurses.  If not, see
;; <http://www.gnu.org/licenses/>.

(use-modules (test automake-test-lib)
             (ncurses curses)
             (srfi srfi-1))

(setlocale LC_ALL "")

;; There isn't really a way to test (meta! #f)
;; using the ungetch/getch technique.
;; Even if (meta!) is false, if you unget
;; a character above 128, you're going to getch
;; a character above 128.

;; I'll leave this test here just to show that I
;; thought about it.

(setlocale LC_ALL "")
(automake-test
 (with-latin1-locale*
  (lambda ()
    (let* ((win (initscr))
	   (TEST_CHAR_VAL (+ 128 64))
	   (TEST_CHAR (integer->char TEST_CHAR_VAL)))
      (clear win)
      (refresh win)
      (nodelay! win #t)
      (meta! #f)
      (ungetch TEST_CHAR_VAL)
      (let ((c (getch win)))
	(endwin)
	(newline)
	(format #t "wide-ncurses? ~s~%" %wide-ncurses)
	(format #t "locale: ~s~%" (setlocale LC_ALL))
	(format #t "TEST_CHAR: ~s~%" TEST_CHAR)
	(format #t "TEST_CHAR_VAL: ~s~%" TEST_CHAR_VAL)
	(format #t "getch: ~S~%" c)
	(format #t "getch as val: ~s~%" (char->integer c))
	'skipped)))))
