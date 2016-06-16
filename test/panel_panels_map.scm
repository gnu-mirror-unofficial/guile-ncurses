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
             (srfi srfi-1)
             (ncurses curses)
             (ncurses panel))

(automake-test
 (let* ((mainwin (initscr))
	(win1 (newwin 5 5 5 5))		; Created 1st, so lower Z-order
	(win2 (newwin 10 10 5 5)))	; Created 2nd, so higher Z-order
	      
   (make-panel! win1)
   (make-panel! win2)
   (let ((plist1 (panels-map (lambda (x) x)))
	 (plist2 (list win2 win1)))
     ;; Both plist1 and plist2 should be in top-down Z-order
     (endwin)
     (newline)
     (format #t "panel1: ~s~%" win1)
     (format #t "panel2: ~s~%" win2)
     (display "plist1: ")
     (write plist1)
     (newline)
     (display "plist2: ")
     (write plist2)
     (newline)
     (list= panel=? plist1 plist2))))
