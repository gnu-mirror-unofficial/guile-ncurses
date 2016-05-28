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
	  (item2 (new-item "item2" "description2"))
	  (item3 (new-item "item3" "description3"))
	  (item4 (new-item "item4" "description4"))
	  (item5 (new-item "item5" "description5"))
	  (m (new-menu (list item1 item2 item3 item4 item5))))

     ;; If we make a menu with space for only 3 item, the last
     ;; two items won't be visible
     (set-menu-format! m 3 1)
     (post-menu m)
     (refresh win)
     (let ((v1 (item-visible? item1))
	   (v2 (item-visible? item2))
	   (v3 (item-visible? item3))
	   (v4 (item-visible? item4))
	   (v5 (item-visible? item5)))
       (unpost-menu m)
       (refresh win)
       (endwin)
       (newline)
       (format #t "item visible? 1: ~s~%" v1)
       (format #t "item visible? 2: ~s~%" v2)
       (format #t "item visible? 3: ~s~%" v3)
       (format #t "item visible? 4: ~s~%" v4)
       (format #t "item visible? 5: ~s~%" v5)
       (and v1 v2 v3 (not v4) (not v5))))))
