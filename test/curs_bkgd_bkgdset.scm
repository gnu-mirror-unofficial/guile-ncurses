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
             (ncurses curses))


;; Requires a terminal that can do color

(automake-test
 (let ((win (initscr)))
   (start-color!)
   (init-pair! 1 COLOR_BLACK COLOR_RED)
   (clear win)
   (standend! win)
   (addstr win "This has default colors and spaces as background" #:y 0 #:x 0)
   (bkgdset! win (color 1 (dim #\+)))
   (addstr win "This is black on red and has plus signs as background"
	   #:y 1 #:x 0)
   (refresh win)
   (maybe-sleep 2)
   (let ((x1 (inch win #:y 0 #:x 4)) ; Loc of space between
					; "This" and "is"
	 (x2 (inch win #:y 1 #:x 4)))
     (endwin)
     (newline)
     (format #t "background char: ~s" x1)
     (newline)
     (format #t "background char: ~s" x2)
     (newline)
     (and
      (xchar-equal? (normal #\space) x1)
      (xchar-equal? (color 1 (dim #\+)) x2)))))
