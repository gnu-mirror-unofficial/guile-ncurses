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

;; If we are echoing, you might think that ungetch/getch should
;; print a char on the screen, but, it doesn't work that way.

;; So really this test doesn't test anything.

(automake-test
 (let ((win (initscr)))
   (clear win)
   (refresh win)
   (cbreak!)
   (noecho!)
   (move win 0 0)
   (ungetch #\x)
   (refresh win)
   (let ((char-get (getch win #:x 0 #:y 0)))
     (refresh win)
     (maybe-sleep 2)
     (let ((ret (inch win #:x 0 #:y 0)))
       (refresh win)
       (maybe-sleep 2)
       (endwin)
       (newline)
       (format #t "getch: ~s~%" char-get)
       (format #t "char: ~s~%" ret)
       (newline)
       (xchar-equal? (normal #\space) ret)
       'skipped))))
