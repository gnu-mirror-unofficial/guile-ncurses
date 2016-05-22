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
	     (srfi srfi-1)
             (ncurses curses))

;; A functional terminal should at least have
;; these common keys.

(automake-test
 (let* ((win (initscr)))
   (cbreak!)
   (echo!)
   (keypad! win #t)
   (let* ((keylist (list
		    KEY_DOWN
		    KEY_UP
		    KEY_LEFT
		    KEY_RIGHT
		    KEY_HOME
		    KEY_BACKSPACE
		    (key-f 1)
		    (key-f 2)
		    (key-f 3)
		    (key-f 4)))
	  (names  (map keyname keylist))
	  (validity (map has-key? keylist)))
     (endwin)
     (newline)
     (every
      (lambda (x) x)
      (fold (lambda (key name valid prev)
	      (if valid
		  (format #t "OK ~s ~s ~%" key name)
		  (format #t "ERR ~s ~s~%" key name))
	      (append prev (list valid)))
	    '()
	    keylist
	    names
	    validity)))))

