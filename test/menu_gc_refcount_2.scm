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
   (let* ((rc2 (%item-refcount item))
	  (item2 (current-item m))
	  (rc3 (%item-refcount item))
	  (rc4 (%item-refcount item2)))
     (unpost-menu m)
     (endwin)
     (newline)
     (format #t "recount item #1 before menu: ~s~%" rc1)
     (format #t "refcount item #1 attached to menu: ~s~%" rc2)
     (format #t "refcount item #1 after get-current-item: ~s~%" rc3)
     (format #t "refcount item #2 after get-current-item: ~s~%" rc4)
     (and
      (= rc1 1)
      (= rc2 2)
      (= rc3 3)
      (= rc4 3)))))
   
