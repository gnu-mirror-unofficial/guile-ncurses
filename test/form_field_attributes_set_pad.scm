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
	     (ice-9 format)
             (ncurses curses)
             (ncurses form))

(automake-test
 (let ((stdscr (initscr)))
   (let* ((field1 (new-field 1 10 0 0 0 0))
	  (field2 (new-field 1 10 1 0 0 0))
	  (field3 (new-field 1 10 2 0 0 0)))

     (set-field-pad! field1 #\sp)
     (set-field-buffer! field1 0 "Space")

     (set-field-pad! field2 #\.)
     (set-field-buffer! field2 0 "Period")

     (set-field-pad! field3 #\+)
     (set-field-buffer! field3 0 "Plus")
     (let* (
	    ;; Post that form
	    (frm (new-form (list field1 field2 field3))))
       (post-form frm)
       ;; Paint it
       (refresh stdscr)
       ;;(sleep 1)
       (let ((ff1 (field-pad field1))
	     (ff2 (field-pad field2))
	     (ff3 (field-pad field3)))
	 (unpost-form frm)
	 (endwin)
	 (newline)
	 (format #t "field1: ~s~%" field1)
	 (format #t "field2: ~s~%" field2)
	 (format #t "field3: ~s~%" field3)
	 (format #t "field-pad1:  ~s~%" ff1)
	 (format #t "field-pad2:  ~s~%" ff2)
	 (format #t "field-pad3:  ~s~%" ff3)
	 (and (eqv? ff1 #\sp)
	      (eqv? ff2 #\.)
	      (eqv? ff3 #\+)))))))
