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

(define hello (normal "hello"))
(define he (normal "he"))
(define hello-bold (bold "hello"))
(define he-bold (bold "he"))

(automake-test
 (let* ((stdscr (initscr)))
   (clear stdscr)
   (move stdscr 0 0)
   (addchstr stdscr hello)
   (refresh stdscr)
   (let ((w2 (inchstr stdscr #:y 0 #:x 0 #:n 10)))
     (endwin)
     (newline)
     (format #t "inchstr: ~s~%" w2)
     (list= xchar-equal? hello (list-head w2 (length hello))))))
