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
	     (ice-9 format)
	     (ncurses curses)
	     (ncurses slk))

(automake-test
 (begin
   (slk-init 0)
   (let ((win (initscr)))
     (start-color!)
     (clear win)
     
     (move win 0 0)
     (addstr win "Setting soft keys to normal")
     (refresh win)
     
     (slk-attr-set! A_NORMAL 0)
     (slk-set 1 "normal" 1)
     (slk-refresh)
     (maybe-sleep 2)

     (move win 1 0)
     (addstr win "Setting soft keys to bold")
     (refresh win)
     
     (slk-attr-set! A_BOLD 0)
     (slk-set 2 "bold" 1)
     (slk-refresh)
     (maybe-sleep 2)
     
     (let* ((rendition (slk-attr))
	    (attributes (car rendition))
	    (color (cadr rendition)))
       (endwin)
       (newline)
       (format #t "  slk-attr: ~32,'0b~%" attributes)
       (format #t "  A_NORMAL: ~32,'0b~%" A_NORMAL)
       (format #t "A_STANDOUT: ~32,'0b~%" A_STANDOUT)
       (format #t " A_REVERSE: ~32,'0b~%" A_REVERSE)
       (format #t "    A_BOLD: ~32,'0b~%" A_BOLD)
       (format #t "color-pair: ~s~%" color)
       ;; Note that on MinGW, there is a bug in upstream ncurses that
       ;; returns 0 for slk-attr xterm.  For some color procedures on
       ;; MinGW, ncurses mistakenly returns the values for
       ;; TERM=#w32con instead of TERM=xterm.
       (or (and
	    (equal? attributes A_BOLD)
	    (equal? color 0))
	   'skipped)))))
 
