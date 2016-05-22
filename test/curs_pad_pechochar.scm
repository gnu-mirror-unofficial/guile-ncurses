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

(automake-test
 (let* ((mainwin (initscr))
	(win (newpad 10 10)))
   (clear mainwin)
   (refresh mainwin)
   (move win 1 1)
   (pechochar win (normal #\1))
   (pechochar win (normal #\2))
   (pechochar win (normal #\3))
   (pechochar win (normal #\4))
   (refresh mainwin)
   (maybe-sleep 1)
   (let ((x1 (instr win #:y 1 #:x 1 #:n 4)))
     (endwin)
     (newline)
     (format #t "instr: ~s (expected ~s)~%" x1 "1234")
     (string=? x1 "1234"))))
