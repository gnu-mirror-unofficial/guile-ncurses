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

(setlocale LC_ALL "")
(automake-test
 (begin
   (if (not (fluid-ref %default-port-encoding))
       (throw 'skipped))
   (if (not (string-contains-ci (fluid-ref %default-port-encoding) "utf"))
       (throw 'skipped))
   
   (let ((win (initscr)))
     (cbreak!)
     (echo!)
     (ungetch #\ż) ; LATIN SMALL LETTER Z WITH DOT ABOVE
     (clear win)
     (refresh win)
     (let ((x1 (getch win)))
       (endwin)
       (newline)
       (format #t "getch: ~s~%" x1)
       (equal? x1 #\ż)))))
 ;; This should work, but some pre-2009 ncurses libraries
 ;; ncurses library can't ungetch non-ASCII chars