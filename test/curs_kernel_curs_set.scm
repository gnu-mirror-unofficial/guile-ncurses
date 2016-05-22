;; Copyright 2009, 2010, 2011, 2013, 2016 Free Software Foundation, Inc.

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

(automake-test
 (let* ((win (initscr)))
   (refresh win)

   ;; Curs-set should not return #f if this terminal has the capability
   ;; to set the visiblity of the cursor.  Not all terminals have this
   ;; capability.
   (let* ((x1 (curs-set 0))
	  (x2 (curs-set 1))
	  (x3 (curs-set 2)))
     ;; This restores the cursor to the original setting.
     (if x1 (curs-set x1))
     (endwin)
     (newline)
     (format #t "curs-set ~s ~s ~s ~%" x1 x2 x3)
     ;; This is not false if the cursor could be set.
     (and x1 x2 x3))))
