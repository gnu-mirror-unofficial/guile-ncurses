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
   (clear win)
   (refresh win)
   (let* ((win2 (false-if-exception (call-with-input-string windata getwin)))
	  (win2-contents (and win2
			      (begin
				(refresh win2)
				(instr win2 #:y 0 #:x 0 #:n 3)))))
     (endwin)
     (newline)
     (format #t "original window ~s~%" win)
     (format #t "reconsituted window ~s~%" win2)
     (format #t "reconsituted window contents ~s~%" win2-contents)
     (if (and (string? win2-contents) (string=? "zzz" win2-contents))
       #t
       ;; But since the return val to putwin is broken is some 6.0 curses
       ;; we won't fail on this, for now.
       'skipped))))
