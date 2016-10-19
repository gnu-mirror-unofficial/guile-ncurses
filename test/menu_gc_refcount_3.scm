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
	     (ncurses curses)
	     (ncurses menu))

(automake-test
 (let* ((mainwin (initscr))
	(item (new-item "item1" "description1"))
	(rc1 (%item-refcount item))
	(m (new-menu (list item))))
   (post-menu m)
   (set-current-item! m item)
   (refresh mainwin)
   (maybe-sleep 2)
   (current-item m)
   (current-item m)
   (current-item m)
   (current-item m)
   (current-item m)
   (gc)
   (let* ((rc2 (%item-refcount item)))
     (unpost-menu m)
     (endwin)
     (newline)
     ;; Should be 1 for new-item
     (format #t "recount item #1 before menu: ~s~%" rc1)
     ;; Could be anything between 2 and 7
     (format #t "refcount item #1 after freeing get-current-item: ~s~%" rc2)
     (and
      (= rc1 1)
      (and (>= rc2 2) (<= rc2 7))))))

   
