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
             (srfi srfi-1)
             (ncurses curses))

(setlocale LC_ALL "C")
(automake-test
 (if (or (string-contains (curses-version) "5.7")
         (string-contains (curses-version) "5.8")
         (string-contains (curses-version) "5.9"))
     'skipped
     (with-utf8-locale*
      (lambda ()
        (let* ((win (initscr)))
          (erase win)
          (refresh win)
          (ungetch #\nl)
          (ungetch #\¶)
          (ungetch #\⅕)
          (ungetch #\ᴁ)
          (let ((s1 (getnstr win 10)))
            (refresh win)
            (maybe-sleep 2)
            (endwin)
            (newline)
            (format #t "getnstr: ~s" s1)
            (newline)
            (string=? s1 "ᴁ⅕¶")))))))
;; This should work, but some pre-2009 ncurses libraries
;; ncurses library can't ungetch non-ASCII chars
