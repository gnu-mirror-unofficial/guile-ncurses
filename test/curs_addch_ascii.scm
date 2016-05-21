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

;; Check to see if we can write and then read the characters
;; from the alternative character set to the screen.

(setlocale LC_ALL "")
(automake-test
 (let* ((win (initscr))
	;; This list needs to be constrained to 24 lines or fewer
	(character-alist
	 `(("SMALL A" . ,(normal #\a))
	   ("LARGE A" . ,(normal #\A))
	   ("SMALL X" . ,(normal #\x))
	   ("LARGE X" . ,(normal #\X))))
	(result-list '()))
   (clear win)

   ;; Write the character names and the special character to the screen
   (do ((i 0 (1+ i)))
       ((>= i (length character-alist)))
     (move win i 0)
     (addstr win (car (list-ref character-alist i)))
     (move win i 20)
     (addch win (cdr (list-ref character-alist i))))
   (refresh win)
   (maybe-sleep 5)

   ;; Read the special characters back from the screen
   (do ((i 0 (1+ i)))
       ((>= i (length character-alist)))
     (newline)
     (move win i 20)
     (set! result-list
	   (append! result-list
		    (list
		     (xchar-equal? (normal (cdr (list-ref character-alist i)))
				   (inch win))))))
   (endwin)
   (newline)


   (do ((i 0 (1+ i)))
       ((>= i (length character-alist)))
     (format #t "name ~s char ~s match ~s~%" (car (list-ref character-alist i))
	     (cdr (list-ref character-alist i))
	     (list-ref result-list i)))
   (every (lambda (x) x) result-list)))
