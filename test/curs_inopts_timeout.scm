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

;; Don't have any real idea how to test timeout!
;; so we just check for its existence.

(automake-test
 (if (not (defined? 'getdelay))
     'skipped
     (let ((win (initscr)))
       (clear win)
       (qiflush!)
       (timeout! win 2)
       (let ((t (getdelay win)))
	 (endwin)
	 (newline)
	 (format #t "getdelay: ~s~%" t)
	 (equal? t 2)))))
