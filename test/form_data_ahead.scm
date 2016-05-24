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
             (ncurses curses)
             (ncurses form))

;; You can make a form with only a single field

(automake-test
 (let* ((mainwin (initscr))
	;; Make a field
	(field1 (new-field 1 10 0 0 0 0))
	;; Let the field have more text than the visible
	(ret1 (field-opts-off! field1 O_STATIC))
	;; Add text to the fields.  More than the screen box can hold
	(ret2 (set-field-buffer! field1 0 "ABCDEFGHIJLKMNOPQRSTUVWXYZ"))
	;; Post that form
	(frm (new-form (list field1)))
	(ret3 (post-form frm))
	;; Paint it
	(ret4 (refresh mainwin))
	;; Check if there are characters to the left of the field (yes)
	(ret7 (data-behind? frm))
	;; Check if there are characters to the right of the field (no)
	(ret8 (data-ahead? frm)))
   (endwin)
   (newline)
   (format #t "field: ~s~%" field1)
   (format #t "form: ~s~%" frm)
   (format #t "data-behind? ~s~%" ret7)
   (format #t "data-ahead? ~s~%" ret8)
   (and (not ret7) ret8)))

