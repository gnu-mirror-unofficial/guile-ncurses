;; Copyright 2009, 2010, 2011, 2016 Free Software Foundation, Inc.

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
             (srfi srfi-1))

;; Should be able to overlay one window onto another

(automake-test
 (let* ((mainwin (initscr))
	(win (newpad 10 10))
	(win2 (subpad win 4 4 3 3)))
   
   (clear mainwin)
   (refresh mainwin)
   (box win2 0 0)
   (addstr win2 "yy" #:y 1 #:x 1)
   (touchwin win)
   (pnoutrefresh win 0 0 5 5 15 15)
   (refresh mainwin)
   (maybe-sleep 1)
   (let ((x1 (instr win2 #:y 1 #:x 1 #:n 2)))
     (endwin)
     (newline)
     (format #t "instr: ~s~%" x1)
     (string=? x1 "yy"))))

