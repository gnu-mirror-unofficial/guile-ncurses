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
             (srfi srfi-1)
             (ice-9 format)
             (ncurses curses))

(automake-test
 (let ((win (initscr)))
   (start-color!)
   (clear win)
   (attr-set! win A_NORMAL)
   (color-set! win 1)
   (addstr win "x" #:y 0 #:x 0)
   (refresh win)
   (let ((x2 (inch win #:y 0 #:x 0)))
     (endwin)
     (newline)
     (format #t "inch: ~s~%" x2)
     (format #t "xchar-color: ~s~%" (xchar-color x2))
     (equal? 1 (xchar-color x2)))))
