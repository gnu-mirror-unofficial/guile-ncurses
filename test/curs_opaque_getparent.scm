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
             (ncurses curses)
             (srfi srfi-1))

(automake-test
 (let* ((win (initscr))
	(parent (newwin 10 10 0 0))
	(child (subwin parent 5 5 0 0)))
   (clear win)
   (refresh win)
   (let ((ret (getparent child)))
     (endwin)
     (newline)
     (format #t "parent: ~s~%" parent)
     (format #t "subwin: ~s~%" child)
     (format #t "getparent: ~s (expected ~s)~%" ret parent)
     (newline)
     ;; (eq? ret parent)
     ;; (eqv? ret parent)
     (equal? ret parent)
     )))
