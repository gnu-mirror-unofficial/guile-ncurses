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
 (let ((stdscr (initscr)))

   ;; Check that the page can be set
   (let* ((field1 (new-field 1 10 0 0 0 0))
	  (field2 (new-field 1 10 1 0 0 0)))
     ;; Put the second field on the second page
     (set-new-page! field2 #t)
     (set-field-buffer! field1 0 "Page 1")
     (set-field-buffer! field2 0 "Page 2")
     (let* ((form1 (new-form (list field1 field2)))
	    (test1 #f)
	    (test2 #f))
       
       ;; Post the first page
       (set-form-page! form1 0)
       (post-form form1)
       ;; Paint it
       (refresh stdscr)
       ;; Get the page number
       (set! test1 (form-page form1))
       (maybe-sleep 1)

       ;; Post the second page
       (set-form-page! form1 1)
       ;; Paint it
       (refresh stdscr)
       ;; Get the page number
       (set! test2 (form-page form1))
       (maybe-sleep 1)
       (unpost-form form1)
       (endwin)
       (newline)
       
       (format #t "field1: ~s~%" field1)
       (format #t "field2: ~s~%" field2)
       (format #t "form: ~s~%" form1)
       (format #t "form-page 1: ~s~%" test1)
       (format #t "form-page 2: ~s~%" test2)
       (and (eq? test1 0)
	    (eq? test2 1))))))
