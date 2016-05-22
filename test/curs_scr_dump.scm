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

;; FIXME: This test is clearly not logically correct
;; with respect to screen restoration.  What
;; really *is* screen restoration?

(automake-test
 (let ((win (initscr))
       (fname (tmpnam)))

   (clear win)
   (addstr win "xxx" #:x 0 #:y 0)
   (refresh win)
   (scr-dump fname)
   (maybe-sleep 2)
   
   (addstr win "yyy" #:x 0 #:y 0)
   (refresh win)
   (maybe-sleep 2)

   (endwin)
   (scr-init fname)
   (doupdate)
   (maybe-sleep 2)
   (let ((x1 (instr win #:n 3 #:x 0 #:y 0)))
     (endwin)
     (newline)
     (delete-file fname)
     (format #t "tmpnam: ~s~%" fname)
     (format #t "instr: ~s~%" x1)
     (string=? "xxx" x1)
     'skipped)))
