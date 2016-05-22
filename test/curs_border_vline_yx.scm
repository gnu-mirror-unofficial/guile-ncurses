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
             (ncurses curses))

(automake-test
 (let ((win (initscr)))
   (clear win)
   (vline win (normal #\!) 2 #:y 1 #:x 1)
   (refresh win)
   (maybe-sleep 2)
   (let ((x1  (inch win #:y 1 #:x 1))
	 (x2  (inch win #:y 2 #:x 1)))
     (endwin)
     (newline)
     (format #t "at (1,1) ~s~%" x1)
     (format #t "at (1,2) ~s~%" x2)
     (and
      (xchar-equal? x1 (normal #\!))
      (xchar-equal? x2 (normal #\!))))))
