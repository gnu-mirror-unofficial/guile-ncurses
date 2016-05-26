;; Copyright 2009, 2010, 2013, 2016 Free Software Foundation, Inc.

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
 (let* ((win (initscr)))
   (clear win)
   (setscrreg! win 0 3)
   (scrollok! win #t)
   (move win 0 0)
   (addstr win "aaa\n")
   (addstr win "bbb\n")
   (addstr win "ccc\n")
   (addstr win "ddd\n")
   (addstr win "eee\n")
   (refresh win)
   (let ((x1 (instr win #:y 0 #:x 0 #:n 3))
	 (x2 (instr win #:y 1 #:x 0 #:n 3))
	 (x3 (instr win #:y 2 #:x 0 #:n 3)))
     (endwin)
     (newline)
     (format #t "instr #1: ~s~%" x1)
     (format #t "instr #2: ~s~%" x1)
     (format #t "instr #3: ~s~%" x1)
     (and
      (string=? x1 "ccc")
      (string=? x2 "ddd")
      (string=? x3 "eee")))))
