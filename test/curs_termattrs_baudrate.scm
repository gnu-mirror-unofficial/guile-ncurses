;; Copyright 2009, 2010, 2011, 2014, 2016 Free Software Foundation, Inc.

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

 ;; baudrate gets its data from the terminal line settings, I think.
 ;; So there is no correct response.  I get 38400 when called 
 ;; interactively in an xterm and 0 when called within nohup.

(define expected-baudrate 38400)

(automake-test
 (let ((win (initscr)))
   (clear win)
   (let ((b (baudrate)))
     (endwin)
     (newline)
     (format #t "Baudrate: ~a (expected ~a)~%" b expected-baudrate)
     (= b expected-baudrate)
     ;; Skip this because there is no correct answer
     'skipped)))
