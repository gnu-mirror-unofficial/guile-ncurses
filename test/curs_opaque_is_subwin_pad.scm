;; Copyright 2011, 2016 Free Software Foundation, Inc.

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
	(pad (newpad 10 10))
	(spad (subpad pad 4 4 3 3)))
   (let ((ret (and (is-subwin? spad) (is-pad? spad))))
     (endwin)
     (newline)
     (format #t "is-subwin? ~s~%" (is-subwin? spad))
     (format #t "is-pad? ~s" (is-pad? pad))
     (newline)
     (if (not (and %is-subwin-broken %is-pad-broken))
	 ret
	 'skipped))))
