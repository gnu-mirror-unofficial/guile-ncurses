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
   (let* ((OLDROW 0)
	  (NEWROW 1)
	  (field1 (new-field 1 10 OLDROW 0 0 0)))
     (set-field-back! field1 A_REVERSE)
     (move-field field1 NEWROW 0)
     (let* ((form1 (new-form (list field1)))
	    (finfo (field-info field1)))
       (post-form form1)
       (refresh stdscr)
       (unpost-form form1)
       (endwin)
       (newline)
       (format #t "field: ~s~%" field1)
       (format #t "form: ~s~%" form1)
       (format #t "finfo: ~s~%" finfo)
       (eqv? (third finfo) NEWROW)))))

