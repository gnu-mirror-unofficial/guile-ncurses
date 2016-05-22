;; Copyright 2009, 2010, 2011, 2013, 2016 Free Software Foundation, Inc.

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

;; If we are detached from a terminal (such as when being
;; called in nohup) this can return #f.

(automake-test
 (let* ((win (initscr)))
   (refresh win)
   (let ((ret1 (savetty))
	 (ret2 (resetty)))
     (endwin)
     (newline)
     (format #t "savetty: ~s~%" ret1)
     (format #t "resetty: ~s~%" ret2)
     'skipped)))
