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
             (ncurses extra))

(automake-test
 (if (not %has-termios)
     (begin
       (format #t "%has-termios = ~s~%" %has-termios)
       (throw 'skipped))
     ;; else
     (begin
       (format #t "%has-termios = ~s~%" %has-termios)
       (format #t "B0=~s bps ~s~%" B0 (speed->bps B0))
       (format #t "B9600=~s bps ~s~%" B9600 (speed->bps B9600))
       (and
        (eqv? 0 (speed->bps B0))
        (eqv? 9600 (speed->bps B9600))))))
