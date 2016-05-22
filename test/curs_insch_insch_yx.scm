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

(setlocale LC_ALL "")

(automake-test
 (let ((win (initscr)))
   (clear win)
   (refresh win)
   (addstr win "xxx" #:y 0 #:x 0)
   (insch win (normal #\z) #:y 0 #:x 0)
   (refresh win)
   (let ((x1 (inch win #:y 0 #:x 0))
	 (x2 (inch win #:y 0 #:x 1)))
     (endwin)
     (newline)
     (write x1)
     (newline)
     (write x2)
     (newline)
     (and
      (xchar-equal? x1 (normal #\z))
      (xchar-equal? x2 (normal #\x))))))
