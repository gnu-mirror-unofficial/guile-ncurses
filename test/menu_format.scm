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
	     (ncurses curses)
	     (ncurses menu))

(automake-test
 (let ((win (initscr)))
   (let* ((item1 (new-item "item1" "description1"))
	  (menu (new-menu (list item1))))
     (set-menu-format! menu 3 2)
     (post-menu menu)
     (refresh win)
     ;; (sleep 3)
     (let* ((rowcol (menu-format menu))
	    (row (first rowcol))
	    (col (second rowcol)))
       (unpost-menu menu)
       (refresh win)
       (endwin)
       (newline)
       (format #t "menu-format ~s~%" rowcol)
       (and (equal? row 3)
	    (equal? col 2))))))
