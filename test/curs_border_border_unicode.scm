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
 (with-utf8-locale*
  (lambda ()
    (let ((win (initscr)))
      (clear win)
      (border win
	      (normal #\│) (normal #\│) (normal #\─) (normal #\─)
	      (normal #\┌) (normal #\┐) (normal #\└) (normal #\┘))
      (refresh win)
      (maybe-sleep 2)
      (let ((l  (inch win #:y 1              #:x 0))
	    (r  (inch win #:y 1              #:x (- (cols) 1)))
	    (t  (inch win #:y 0              #:x 1))
	    (b  (inch win #:y (- (lines) 1)  #:x 1))
	    (tl (inch win #:y 0              #:x 0))
	    (tr (inch win #:y 0              #:x (- (cols) 1)))
	    (bl (inch win #:y (- (lines) 1)  #:x 0))
	    (br (inch win #:y (- (lines) 1)  #:x (- (cols) 1))))
	(endwin)
	(newline)
	(format #t "left ~s~%" l)
	(format #t "right ~s~%" r)
	(format #t "top ~s~%" t)
	(format #t "bottom ~s~%" b)
	(format #t "top-left ~s~%" tl)
	(format #t "top-right ~s~%" tr)
	(format #t "bottom-left ~s~%" bl)
	(format #t "bottom-right ~s~%" br)
	(and
	 ;; These are the unicode values
	 ;; U+2502 BOX DRAWINGS LIGHT VERTICAL
	 (xchar-equal? l (normal #\│))
	 (xchar-equal? r (normal #\│))
	 ;; U+2500 BOX DRAWINGS LIGHT HORIZONTAL
	 (xchar-equal? t (normal #\─))
	 (xchar-equal? b (normal #\─))
	 ;; U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT
	 (xchar-equal? tl (normal #\┌))
	 ;; U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT
	 (xchar-equal? tr (normal #\┐))
	 ;; U+2514 BOX DRAWINGS LIGHT UP AND RIGHT
	 (xchar-equal? bl (normal #\└))
	 ;; U+2518 BOX DRAWINGS LIGHT UP AND LEFT
	 (xchar-equal? br (normal #\┘))))))))
