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
	  (field3 (new-field 1 10 2 0 0 0))
	  (field4 (new-field 1 10 3 0 0 0))
	  (field5 (new-field 1 10 4 0 0 0))
	  (field6 (new-field 1 10 5 0 0 0))
	  (field7 (new-field 1 10 6 0 0 0)))
     
     (set-field-fore! field1 A_BLINK)
     (set-field-buffer! field1 0 "Blink")
     
     (set-field-fore! field2 A_BOLD)
     (set-field-buffer! field2 0 "Bold")
     
     (set-field-fore! field3 A_DIM)
     (set-field-buffer! field3 0 "Dim")

     (set-field-fore! field4 A_INVIS)
     (set-field-buffer! field4 0 "Invis")

     (set-field-fore! field5 A_NORMAL)
     (set-field-buffer! field5 0 "Normal")

     (set-field-fore! field6 A_REVERSE)
     (set-field-buffer! field6 0 "Reverse")

     (set-field-fore! field7 A_UNDERLINE)
     (set-field-buffer! field7 0 "Underline")

     (let* (
	    ;; Post that form
	    (frm (new-form (list field1 field2 field3 field4 field5 field6 field7))))
       (post-form frm)
       ;; Paint it
       (refresh stdscr)
       ;;(sleep 1)
       (let ((ff1 (field-fore field1))
	     (ff2 (field-fore field2))
	     (ff3 (field-fore field3))
	     (ff4 (field-fore field4))
	     (ff5 (field-fore field5))
	     (ff6 (field-fore field6))
	     (ff7 (field-fore field7)))
	 (unpost-form frm)
	 (endwin)
	 (newline)
	 (format #t "field1: ~s~%" field1)
	 (format #t "field2: ~s~%" field2)
	 (format #t "field3: ~s~%" field3)
	 (format #t "field4: ~s~%" field4)
	 (format #t "field5: ~s~%" field5)
	 (format #t "field6: ~s~%" field6)
	 (format #t "field7: ~s~%" field7)
	 (format #t "    A_BLINK:  ~32,'0b~%" A_BLINK)
	 (format #t "     A_BOLD:  ~32,'0b~%" A_BOLD)
	 (format #t "      A_DIM:  ~32,'0b~%" A_DIM)
	 (format #t "    A_INVIS:  ~32,'0b~%" A_INVIS)
	 (format #t "   A_NORMAL:  ~32,'0b~%" A_NORMAL)
	 (format #t "  A_REVERSE:  ~32,'0b~%" A_REVERSE)
	 (format #t "A_UNDERLINE:  ~32,'0b~%" A_UNDERLINE)
	 (format #t "field-fore1:  ~32,'0b~%" ff1)
	 (format #t "field-fore2:  ~32,'0b~%" ff2)
	 (format #t "field-fore3:  ~32,'0b~%" ff3)
	 (format #t "field-fore4:  ~32,'0b~%" ff4)
	 (format #t "field-fore5:  ~32,'0b~%" ff5)
	 (format #t "field-fore6:  ~32,'0b~%" ff6)
	 (format #t "field-fore7:  ~32,'0b~%" ff7)
	 (and (eqv? (field-fore field1) A_BLINK)
	      (eqv? (field-fore field2) A_BOLD)
	      (eqv? (field-fore field3) A_DIM)
	      (eqv? (field-fore field4) A_INVIS)
	      (eqv? (field-fore field5) A_NORMAL)
	      (eqv? (field-fore field6) A_REVERSE)
	      (eqv? (field-fore field7) A_UNDERLINE)))))))
