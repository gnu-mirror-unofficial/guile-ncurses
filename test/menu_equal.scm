;;;; -*- Mode: scheme; -*-

;;;; menu_equal.scm

;; Copyright 2019 Free Software Foundation, Inc.

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
             (ice-9 format)
             (srfi srfi-1)
             (ncurses curses)
             (ncurses menu))

(automake-test
 (let* ((win (initscr))
	(item1 (new-item "item1" "unselected"))
	(item2 (new-item "item2" "unselectable"))
	(item3 (new-item "item3" "selected"))
	(item4 (new-item "item4" "unselected"))
	(menu (new-menu (list item1 item2 item3 item4))))
   (post-menu menu)
   (refresh win)
   (let ((test1 (menu=? menu menu))
         (test2 (item=? item1 item1))
         (test3 (not (item=? item1 item2))))
     (endwin)
     (newline)
     (format #t "menu: ~s~%" menu)
     (format #t "test: ~s ~s ~s ~%" test1 test2 test3)
     (and test1 test2 test3))))
