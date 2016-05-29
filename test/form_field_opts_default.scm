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
             (ncurses form)
             (ice-9 format))

(automake-test
 (let ((stdscr (initscr)))
   (let* ((field1 (new-field 1 10 0 0 0 0))
	  (fo (field-opts field1))
	  (fo2 (logior O_VISIBLE
		       O_ACTIVE
		       O_PUBLIC
		       O_EDIT
		       O_WRAP
		       O_BLANK
		       O_AUTOSKIP
		       O_NULLOK
		       O_STATIC
		       O_PASSOK)))
     (endwin)
     (newline)
     (format #f "VISIBL ~16,'0b~%" O_VISIBLE)
     (format #f "ACTIVE ~16,'0b~%" O_ACTIVE)
     (format #f "PUBLIC ~16,'0b~%" O_PUBLIC)
     (format #f "  EDIT ~16,'0b~%" O_EDIT)
     (format #f "  WRAP ~16,'0b~%" O_WRAP)
     (format #f " BLANK ~16,'0b~%" O_BLANK)
     (format #f "  SKIP ~16,'0b~%" O_AUTOSKIP)
     (format #f "NULLOK ~16,'0b~%" O_NULLOK)
     (format #f "STATIC ~16,'0b~%" O_STATIC)
     (format #f "PASSOK ~16,'0b~%" O_PASSOK)
     (format #f "DEFALT ~16,'0b~%" fo)
     (eqv? fo fo2))))
