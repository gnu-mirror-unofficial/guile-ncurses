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

(automake-test
 (let* ((mainwin (initscr))
	(win (newwin 5 6 7 8))
	(win2 (subwin win 2 2 8 9)))
   (clear mainwin)
   (refresh mainwin)
   (box win 0 0)
   (box win2 0 0)
   (touchwin win )
   (refresh win2)
   (refresh win)
   (maybe-sleep 2)
   (let ((pyx (getbegyx win)))
     (endwin)
     (format #t "parent: ~s~%" win)
     (format #t "getbegyx: ~s~%" pyx)
     (list= eqv? '(7 8) pyx))))
