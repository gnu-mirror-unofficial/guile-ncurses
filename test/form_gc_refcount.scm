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

(define (show-one-field-form-get-refcount win field)
  (let* ((m (new-form (list field)))
         (rc (%field-refcount field)))
    (post-form m)
    (set-current-field! m field)
    (refresh win)
    (maybe-sleep 2)
    (unpost-form m)
    rc))

(automake-test
 (let* ((mainwin (initscr))
	(field (new-field 1 10 0 0 0 0))
	(rc1 (%field-refcount field))
	(rc2 (show-one-field-form-get-refcount mainwin field)))
   (gc)
   (let ((rc3 (%field-refcount field)))
     (endwin)
     (newline)

     ;; A new field should have a refcount of 1
     (format #t "recount as field: ~s~%" rc1)

     ;; When attached to a form, its refcount should be 2
     (format #t "refcount attached to form: ~s~%" rc2)

     ;; After detaching from the form, the refcount
     ;; might be reduced to one, if the GC is fast enough.
     (format #t "refcount after gc: ~s~%" rc3)
     (and
      (= rc1 1)
      (= rc2 2)
      (or (= rc3 2) (= rc3 1))))))
