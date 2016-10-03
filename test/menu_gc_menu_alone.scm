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

        ;; Make a menu without keeping a separate reference to the
        ;; menu items.
        (my-menu (new-menu (list (new-item "item1" "description1")
                                 (new-item "item2" "descritpion2")))))
   ;; Hit the garbage collector
   (gc)

   ;; Menu should post sucessfully, even though the temporary
   ;; items created by new-item may have been collected.
   (post-menu my-menu)
   (refresh mainwin)
   (maybe-sleep 2)

   ;; We're probably already on the first item, but, explicitly
   ;; move to the first item, and grab it.
   (menu-driver my-menu REQ_FIRST_ITEM)
   (let ((item1 (current-item my-menu)))
     (refresh mainwin)
     (maybe-sleep 2)

     ;; Now get the next item.
     (menu-driver my-menu REQ_NEXT_ITEM)
     (let ((item2 (current-item my-menu)))
       (refresh mainwin)
       (maybe-sleep 2)

       (unpost-menu my-menu)
       (endwin)
       (newline)

       ;; If everything has worked as expected, these menu items
       ;; that we got from current-item calls should have a refcount
       ;; of two or three. One from the current-item and one that
       ;; binds it to my-menu, and maybe one from the new-item.
       (format #t "menu: ~s~%" my-menu)
       (format #t "item1: ~s~%" item1)
       (format #t "refcount item 1: ~s~%" (%item-refcount item1))
       (format #t "item2: ~s~%" item2)
       (format #t "refcount item 2: ~s~%" (%item-refcount item2))
       (and
        (string=? "item1" (item-name item1))
        (string=? "item2" (item-name item2))
        (or (= 2 (%item-refcount item1)) (= 3 (%item-refcount item1)))
        (or (= 2 (%item-refcount item2)) (= 3 (%item-refcount item2))))))))
