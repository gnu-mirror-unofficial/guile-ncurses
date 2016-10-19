;; Copyright 2016 Free Software Foundation, Inc.

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

(setlocale LC_ALL "")
(automake-test
 (begin
   (let ((mainwin (initscr)))
     (start-color!)
     (let ((ret (assume-default-colors COLOR_BLACK COLOR_YELLOW)))
       (endwin)
       (newline)
       (format #t "assume-default-colors: ~s" ret)
       (newline)
       ;; Note that on MinGW, there is a bug in upstream ncurses that
       ;; returns #f for assume-default-colors on xterm.  For some
       ;; color procedures on MinGW, ncurses mistakenly returns the
       ;; values for TERM=#w32con instead of TERM=xterm.
       (or ret 'skipped)))))
