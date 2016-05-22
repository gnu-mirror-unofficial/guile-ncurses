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

(automake-test
 (let ((win (initscr)))
   (clear win)
   (refresh win)
   (halfdelay! 1)
   (flushinp)
   (let* ((c (getch win))
	  (pass (equal? #f c)))
     (endwin)
     (newline)
     (format #t "getch: ~S~%" c)
     (if pass
	 #t
	 ;; else
	 (begin
	   (display "WARNING: this test has failed,  but, this may not
indicate a true problem.  It may report a false failure if you were
generating keypresses or mouse clicks when the test was running.
")
	   'skipped)))))
