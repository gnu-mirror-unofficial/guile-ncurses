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
             (srfi srfi-13)
             (ncurses curses)
             (ncurses menu))


(automake-test
 (let* ((win (initscr))
	(item1 (new-item "item1" "description1"))
	(item2 (new-item "item2" "description2"))
	(m (new-menu (list item1 item2))))
   (post-menu m)
   (refresh win)
   ;; (sleep 1)
   
   (let ((n1 (item-name item1))
	 (n2 (item-name item2))
	 (d1 (item-description item1))
	 (d2 (item-description item2)))
     (unpost-menu m)
     (refresh win)
     (endwin)
     (newline)
     (format #t "item-name 1: ~s~%" n1)
     (format #t "item-name 2: ~s~%" n2)
     (format #t "item-description 1: ~s~%" d1)
     (format #t "item-description 2: ~s~%" d2)
     (and (string=? n1 "item1")
	  (string=? n2 "item2")
	  (string=? d1 "description1")
	  (string=? d2 "description2")))))
