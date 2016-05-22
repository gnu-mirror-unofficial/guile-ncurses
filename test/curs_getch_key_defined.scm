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
	     (srfi srfi-1)
             (ncurses curses))

;; Let's see if we can redefine KEY_SDL to be ESC~~~

(automake-test
 (let* ((win (initscr)))
   (cbreak!)
   (echo!)
   (keypad! win #t)
   (if (defined? 'key-defined)
       (begin
	 (let* ((x (string #\esc #\~ #\~ #\~))
		(s1 (key-defined x))
		(s2 (define-key x KEY_SDL))
		(s3 (key-defined x)))
	   (endwin)
	   (newline)
	   (format #t "key string ~s~%" x)
	   (format #t "key defined ~s~%" s1)
	   (format #t "define key ~s~%" s2)
	   (format #t "key defined ~s~%" s3)
	   (newline)
	   (and
	    (not s1)
	    (eqv? s3 KEY_SDL))))
       ;; else
       (begin
	 (endwin)
	 (newline)
	 (format #t "untested~%")
	 'skipped))))
