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
             (ncurses curses)
             (ncurses form))

(automake-test
 (let* ((stdscr (initscr))
	(field (new-field 1 10 0 0 0 0)))
   (set-field-fore! field A_BLINK)
   (set-field-buffer! field 0 "Blink")
   (let ((frm (new-form (list field))))
     (post-form frm)
     (refresh stdscr)
     (maybe-sleep 2)
     (let ((ret (field-fore field)))
       (unpost-form frm)
       (endwin)
       (newline)
       (format #t "new-form: ~s~%" frm)
       (format #t "field: ~s~%" field)
       (format #t "A_BLINK: ~s~%" A_BLINK)
       (format #t "field-fore: ~s~%" ret)
       (eqv? ret A_BLINK)))))
