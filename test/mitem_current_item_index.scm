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
   ;; Make the 3rd row the top-most visible row
   (refresh win)
   ;;(sleep 1)
   (let ((index1 (item-index item1))
	 (index2 (item-index item2))
	 (index3 (item-index item3))
	 (index4 (item-index item4))
	 (index5 (item-index item5)))
     (unpost-menu m)
     (refresh win)
     (endwin)
     (format #t "item1: ~s  item-index1: ~s~%" item1 index1)
     (format #t "item2: ~s  item-index2: ~s~%" item2 index2)
     (format #t "item3: ~s  item-index3: ~s~%" item3 index3)
     (format #t "item4: ~s  item-index4: ~s~%" item4 index4)
     (format #t "item5: ~s  item-index5: ~s~%" item5 index5)
     (and (equal? (item-index item1) 0)
	  (equal? (item-index item2) 1)
	  (equal? (item-index item3) 2)
	  (equal? (item-index item4) 3)
	  (equal? (item-index item5) 4)))))
