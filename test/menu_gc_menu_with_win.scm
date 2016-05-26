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

(automake-test
 (let* ((mainwin (initscr))
	(my-menu (new-menu (list (new-item "item1" "description1")
				 (new-item "item2" "description2")))))
   
   ;; Connect the menu to a window
   (set-menu-win! my-menu (newwin 10 40 2 2 #:panel #t))
   (set-menu-sub! my-menu (subwin (menu-win my-menu) 8 38 3 3 #:panel #t))
   (post-menu my-menu)
   (update-panels)
   (doupdate)
   (refresh mainwin)
   (maybe-sleep 2)
   
   ;; Hit that garbage collector
   (gc)
   
   ;; If we're here, the menu should still be attached to a window
   ;; that is 10x40 in size.
   (let* ((win (menu-win my-menu))
	  (sub (menu-sub my-menu))
	  (siz (getmaxyx win))
	  (sizsub (getmaxyx sub)))
     (endwin)
     (newline)
     (format #t "menu: ~s~%" my-menu)
     (format #t "win: ~s~%" win)
     (format #t "sub: ~s~%" sub)
     (format #t "size: ~s~%" siz)
     (format #t "subsize: ~s~%" sizsub)
     (and
      (list= = '(10 40) siz)
      (list= = '(8 38) sizsub)))))
