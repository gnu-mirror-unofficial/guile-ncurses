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
	     (ncurses form))

(automake-test
 (let* ((mainwin (initscr))

        ;; Make a form without keeping a separate reference to
        ;; fields.
        (my-form (new-form (list (new-field 1 10 0 0 0 0)
                                 (new-field 2 10 0 0 0 0)))))

   ;; Hit the garbage collector
   (gc)

   ;; Form should post successfully, even though the temporary
   ;; items created by new-field may have been collected
   (post-form my-form)
   (refresh mainwin)
   (maybe-sleep 2)

   ;; We're probably already in the first field, but, explicitly
   ;; move to the first field, and grab it
   (form-driver my-form REQ_FIRST_FIELD)
   (let ((field1 (current-field my-form)))
     (refresh mainwin)
     (maybe-sleep 2)

     ;; Now move to the next field.
     (form-driver my-form REQ_NEXT_FIELD)
     (let ((field2 (current-field my-form)))
       (refresh mainwin)
       (maybe-sleep 2)

       (unpost-form my-form)
       (endwin)
       (newline)

       ;; If everything has worked as expected, thesefields
       ;; that we got with current-field calls should
       ;; have a refcount of two or three. One from the
       ;; new-form call. One from the current-field call.
       ;; And maybe one from the new-field call.
       (let ((rc1 (%field-refcount field1))
             (rc2 (%field-refcount field2)))
         (format #t "form: ~s~%" my-form)
         (format #t "field1: ~s~%" field1)
         (format #t "refcount field1: ~s~%" rc1)
         (format #t "field2: ~s~%" field2)
         (format #t "refcount field2: ~s~%" rc2)
         (free-form my-form)
         (gc)
         (and
          (or (= 2 rc1) (= 3 rc1))
          (or (= 2 rc2) (= 3 rc2))))))))
