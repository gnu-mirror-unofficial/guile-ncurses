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
             (srfi srfi-1))

;; Should be able to overlay one window onto another

(automake-test
 (let* ((mainwin (initscr))
	(win (newwin 5 5 5 5)))
   (clear win)
   (refresh win)
   (clear mainwin)
   (refresh mainwin)
   (addstr win "xxx" #:y 0 #:x 0)
   (refresh win)

   (overwrite win mainwin)
   (delwin win)
   (refresh mainwin)

   (let ((x1 (instr mainwin #:y 5 #:x 5 #:n 3)))
     (endwin)
     (newline)
     (format #t "instr: ~s~%" x1)
     (string=? x1 "xxx"))))

