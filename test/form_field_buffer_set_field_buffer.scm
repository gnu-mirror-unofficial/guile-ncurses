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
             (srfi srfi-13)
             (ncurses curses)
             (ncurses form))

(automake-test
 (let ((stdscr (initscr)))
   (begin
     (let ((field1 (new-field 1 10 0 0 0 0)))
       (field-opts-off! field1 O_STATIC)
       (let ((form1 (new-form (list field1)))
	     (str #f))
	 (set-field-buffer! field1 0 "Blammo!")
	 (post-form form1)
	 (refresh stdscr)
	 (set! str (field-buffer field1 0))
	 (addstr stdscr (string-append "\"" str "\"") #:y 5 #:x 0)
	 (refresh stdscr)
	 ;;(sleep 1)
	 (endwin)
	 (string=? (substring str 0 (string-length "Blammo!"))
		   "Blammo!"))))))
