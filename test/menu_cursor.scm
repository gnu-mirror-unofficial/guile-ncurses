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
     (post-menu menu)

     ;; Move the cursor away from the menu
     (move win 10 0)
     (addstr win "blammo!")

     ;; Move the cursor back to the menu
     (pos-menu-cursor menu)
     (refresh win)

     (let* ((yx (getyx win))
	    (y (first yx))
	    (x (second yx)))
       (unpost-menu menu)
       (refresh win)
       (endwin)
       (newline)
       (format #t "getyx: ~s~%" yx)

       ;; The test passes if we're back at the menu
       (equal? y 0)))))
