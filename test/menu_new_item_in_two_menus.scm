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
 (let* ((mainwin (initscr))
	(my-item (new-item "item1" "description1"))
	(my-menu-1 (false-if-exception (new-menu (list my-item))))
	(my-menu-2 (false-if-exception (new-menu (list my-item)))))
   (endwin)
   (newline)
   (format #t "my-item: ~s~%" my-item)
   (format #t "my-menu-1: ~s~%" my-menu-1)
   (format #t "my-menu-2: ~s~%" my-menu-2)
   (and
    (item? my-item)
    (menu? my-menu-1)
    (not (menu? my-menu-2)))))
