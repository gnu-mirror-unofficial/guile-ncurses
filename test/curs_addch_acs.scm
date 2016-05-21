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
	 `(("acs-ulcorner" . ,(acs-ulcorner))
	   ("acs-llcorner" . ,(acs-llcorner))
	   ("acs-urcorner" . ,(acs-urcorner))
	   ("acs-lrcorner" . ,(acs-lrcorner))
	   ("acs-rtee" . ,(acs-rtee))
	   ("acs-ltee" . ,(acs-ltee))
	   ("acs-btee" . ,(acs-btee))
	   ("acs-ttee" . ,(acs-ttee))
	   ("acs-hline" . ,(acs-hline))
	   ("acs-vline" . ,(acs-vline))
	   ("acs-plus" . ,(acs-plus))
	   ("acs-s1" . ,(acs-s1))
	   ("acs-s9" . ,(acs-s9))
	   ("acs-diamond" . ,(acs-diamond))
	   ("acs-ckboard" . ,(acs-ckboard))
	   ("acs-degree" . ,(acs-degree))
	   ("acs-plminus" . ,(acs-plminus))
	   ("acs-larrow" . ,(acs-larrow))
	   ("acs-rarrow" . ,(acs-rarrow))
	   ("acs-uarrow" . ,(acs-uarrow))
	   ("acs-darrow" . ,(acs-darrow))
	   ("acs-board" . ,(acs-board))
	   ("acs-lantern" . ,(acs-lantern))
	   ("acs-block" . ,(acs-block))))
	(result-list '()))
   (clear win)
   (do ((i 0 (1+ i)))
       ((>= i (length character-alist)))
     (move win i 0)
     (addstr win (car (list-ref character-alist i)))
     (move win i 20)
     (addch win (cdr (list-ref character-alist i))))
   (refresh win)
   (maybe-sleep 5)
   (do ((i 0 (1+ i)))
       ((>= i (1- (length character-alist))))
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
       ((>= i (1- (length character-alist))))
     (format #t "name ~s char ~s match ~s~%" (car (list-ref character-alist i))
	     (cdr (list-ref character-alist i))
	     (list-ref result-list i)))
   (every (lambda (x) x) result-list)))
