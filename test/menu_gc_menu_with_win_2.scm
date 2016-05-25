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
	     (srfi srfi-1)
             (ncurses curses)
	     (ncurses panel)
             (ncurses menu))

;; Menus can be attached to specified windows.  Those windows
;; need survive garbage collection while the menu exists.

(define (post-then-drop-menu mainwin)
  (let* ((my-menu (new-menu (list (new-item "item1" "description1")
				  (new-item "item2" "description2")))))
    (set-menu-win! my-menu (newwin 10 40 2 2
				   #:panel #t #:name "outer-win"))
    (set-menu-sub! my-menu (subwin (menu-win my-menu) 8 38 3 3
				   #:panel #t #:name "inner-subwin"))
    (post-menu my-menu)
    (update-panels)
    (doupdate)
    (refresh mainwin)
    (maybe-sleep 2)
    (unpost-menu my-menu)))

(automake-test
 (let* ((mainwin (initscr)))
   (post-then-drop-menu mainwin)
   (endwin)

   ;; Hit that garbage collector
   (gc)
   
   ;; If we're here, there wasn't a big crash.
   #t))
