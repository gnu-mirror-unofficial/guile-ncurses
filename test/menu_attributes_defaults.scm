;;;; -*- Mode: scheme; -*-

;;;; m007_menu_attributes.test --- set-menu-fore set-menu-back
;;;; set-menu-grey menu-fore menu-back menu-grey

;; Copyright 2009, 2010 Free Software Foundation, Inc.

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
   (item-opts-off! item2 O_SELECTABLE)
   (menu-opts-off! menu O_ONEVALUE)
   (set-item-value! item1 #f)
   (set-item-value! item3 #t)
   (post-menu menu)
   (refresh win)
   (let ((fore (menu-fore menu))
	 (back (menu-back menu))
	 (grey (menu-grey menu)))
     (endwin)
     (newline)
     (format #t "A_STANDOUT: ~s~%" A_STANDOUT)
     (format #t "A_REVERSE: ~s~%" A_REVERSE)
     (format #t "menu-fore: ~s~%" fore)
     (format #t "A_NORMAL: ~s~%" A_NORMAL)
     (format #t "menu-back: ~s~%" back)
     (format #t "A_UNDERLINE: ~s~%" A_UNDERLINE)
     (format #t "menu-grey: ~s~%" grey)
     (and
      ;; There is a bug in older ncurses that sets
      ;; the default foreground to A_REVERSE,
      ;; not A_STANDOUT, so this test may fail
      ;; unnecessarily
      ;; (equal? (menu-fore menu) A_STANDOUT)
      
      (equal? (menu-back menu) A_NORMAL)
      (equal? (menu-grey menu) A_UNDERLINE)))))

