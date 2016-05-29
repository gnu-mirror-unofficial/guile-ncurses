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

;; Move an unposted field from row 0 to row 1

(automake-test
 (let ((stdscr (initscr)))
   (let* ((field1 (new-field 1 10 0 0 0 0))
	  (field2 (new-field 1 10 1 0 0 0))
	  ;; construct the form using just the 1st field
	  (form1 (new-form (list field1))))
     ;; Now, use both fields
     (set-form-fields! form1 (list field1 field2))
     (post-form form1)
     (refresh stdscr)
     (let ((fc (field-count form1)))
       (unpost-form form1)
       (endwin)
       (newline)
       (format #t "field1: ~s~%" field1)
       (format #t "field2: ~s~%" field2)
       (format #t "form: ~s~%" form1)
       (format #t "field-count: ~s~%" fc)
       (eq? fc 2)))))
