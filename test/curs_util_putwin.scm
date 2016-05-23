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
             (ncurses curses))

(automake-test
 (let* ((mainwin (initscr))
	(win (newwin 5 5 5 5))
	(windata #f))
   (addstr win "zzz" #:y 0 #:x 0)
   (refresh win)
   (set! windata (call-with-output-string
		  (lambda (port)
		    (putwin win port))))
   (endwin)
   (newline)
   (format #t "~s~%" windata)
   (if (and (string? windata) (not (string-null? windata)))
       #t
       ;; But since the return val to putwin is broken is some 6.0 curses
       ;; we won't fail on this, for now.
       'skipped)))
