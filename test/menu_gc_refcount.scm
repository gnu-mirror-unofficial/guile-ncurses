;; Copyright 2016, 2019 Free Software Foundation, Inc.

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

(define (show-one-item-menu-get-refcount win item)
  (let ((m (new-menu (list item)))
        (rc (%item-refcount item)))
    (post-menu m)
    (set-current-item! m item)
    (refresh win)
    (maybe-sleep 2)
    (unpost-menu m)
    rc))

(automake-test
 (let* ((mainwin (initscr))
        (item (new-item "item1" "description1"))
        (rc1 (%item-refcount item))
        (rc2 (show-one-item-menu-get-refcount mainwin item)))
   (begin
     (gc)
     (let ((rc3 (%item-refcount item)))
       (endwin)
       (newline)
       (format #t "recount as item: ~s~%" rc1)
       (format #t "refcount attached to menu: ~s~%" rc2)
       (format #t "refcount after gc: ~s~%" rc3)
       (and
        (= rc1 1)
        (= rc2 2)
        (<= rc3 1))))))
