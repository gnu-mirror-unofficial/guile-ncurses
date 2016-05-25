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

(define (make-then-drop-menu win items)
  (let ((m (new-menu items)))
    (post-menu m)
    (refresh win)
    (maybe-sleep 2)
    (unpost-menu m)))

;; Here we make a menu without holding any other reference
;; to the items used to create it.  The menu's internal copy
;; of the items should survive an instance of garbage collection.

(automake-test
 (let* ((mainwin (initscr)))
   (make-then-drop-menu mainwin (list (new-item "item1" "description1")
				      (new-item "item2" "description2")))

   ;; Hit that garbage collector
   (gc)
   
   ;; If we're here, all the menus and items should have been
   ;; garbage collected.
   #t))
