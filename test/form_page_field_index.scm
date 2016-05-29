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
             (srfi srfi-1)
             (ncurses curses)
             (ncurses form))

(automake-test
 (let ((stdscr (initscr)))

   ;; Check that the form's field can be set by reference
   (let* ((field1 (new-field 1 10 0 0 0 0))
	  (field2 (new-field 1 10 1 0 0 0))
	  (form1 (new-form (list field1 field2))))
     ;; Post that form
     (post-form form1)
     (refresh stdscr)
     ;; Set the current field by reference to the 2nd field
     (set-current-field! form1 field2)
     (refresh stdscr)
     (let ((fi (field-index field2)))
       (unpost-form form1)
       (endwin)
       (newline)
       (format #t "field1: ~s~%" field1)
       (format #t "field2: ~s~%" field2)
       (format #t "form: ~s~%" form1)
       (format #t "field-index: ~s~%" fi)
       ;; Make sure we're pointed at the 2nd field
       (eqv? fi 1)))))
 
