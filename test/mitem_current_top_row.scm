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
	     (ncurses menu))

(automake-test
 (let* ((win (initscr))
	(item1 (new-item "item1" "description1"))
	(item2 (new-item "item2" "description2"))
	(item3 (new-item "item3" "description3"))
	(item4 (new-item "item4" "description4"))
	(item5 (new-item "item5" "description5"))
	(m (new-menu (list item1 item2 item3 item4 item5))))
   (post-menu m)
   (refresh win)
   ;;(sleep 1)
   (let ((top (top-row m)))
     (unpost-menu m)
     (refresh win)
     (endwin)
     (format #t "item1: ~s~%" item1)
     (format #t "top-row: ~s~%" top)
     (equal? top 0))))
