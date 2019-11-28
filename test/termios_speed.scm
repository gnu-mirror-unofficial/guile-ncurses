;; Copyright 2019 Free Software Foundation, Inc.

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
       (let ((T (new-termios)))
         (termios-ispeed-set! T 300)
         (let ((speed (termios-ispeed-get T)))
           (format #t "new-termios: ~s~%" T)
           (format #t "speed: ~s~%" speed)
           (equal? speed 300))))))
