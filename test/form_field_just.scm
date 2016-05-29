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
 (let ((mainwin (initscr)))
   (let ((field1 (new-field 1 10 0 0 0 0))
	 (field2 (new-field 1 10 1 0 0 0))
	 (field3 (new-field 1 10 2 0 0 0))
	 (field4 (new-field 1 10 3 0 0 0)))
     (set-field-just! field1 NO_JUSTIFICATION)
     (set-field-just! field2 JUSTIFY_RIGHT)
     (set-field-just! field3 JUSTIFY_LEFT)
     (set-field-just! field4 JUSTIFY_CENTER)
     (endwin)
     (newline)
     (format #t "NO_JUSTIFICATION: ~s~%" NO_JUSTIFICATION)
     (format #t "JUSTIFY_RIGHT: ~s~%" JUSTIFY_RIGHT)
     (format #t "JUSTIFY_LEFT: ~s~%" JUSTIFY_LEFT)
     (format #t "JUSTIFY_CENTER: ~s~%" JUSTIFY_CENTER)
     (format #t "field-just 1: ~s~%" (field-just field1))
     (format #t "field-just 2: ~s~%" (field-just field2))
     (format #t "field-just 3: ~s~%" (field-just field3))
     (format #t "field-just 4: ~s~%" (field-just field4))
     (and (eqv? (field-just field1) NO_JUSTIFICATION)
	  (eqv? (field-just field2) JUSTIFY_RIGHT)
	  (eqv? (field-just field3) JUSTIFY_LEFT)
	  (eqv? (field-just field4) JUSTIFY_CENTER)))))
