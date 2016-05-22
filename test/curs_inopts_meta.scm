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

;; This test shows that 8-bit characters are not
 ;; truncated when meta is true.

 ;; Also, prior to 20091010, unget_wch had a sign
 ;; bug.  Calling unget_wch with a wchar value above
 ;; 128 made garbage, so this test shouldn't be runn
 ;; in the wide ncurses case.

(automake-test
 (let ((win (initscr)))
   (clear win)
   (refresh win)
   (nodelay! win #t)
   (meta! #t)
   (ungetch (integer->char (+ 128 64)))
   (let* ((c (getch win))
	  (pass (equal? (integer->char (+ 128 64)) c)))
     (endwin)
     (newline)
     (format #t "getch: ~S~%" c)
     (format #t "pass: ~S~%" pass)
     pass)))
