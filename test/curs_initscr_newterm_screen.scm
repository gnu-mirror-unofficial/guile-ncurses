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
             (ncurses curses)
             (ice-9 format)
             (srfi srfi-1))

;; Here we open a terminal screen that reads and writes to a file
;; instead of to the screen.  Normally you'd only be doing this on
;; /dev/ttyS1 or whatever, but, not for this test

;; The newterm procedure should return a #<screen>

(automake-test
 (if (defined? 'newterm)
     (let* ((tmpl (string-copy "gucuXXXXXX"))
	    (input-port (mkstemp! tmpl))
	    (output-port input-port)
	    (scr (newterm "xterm" output-port input-port)))
       (endwin)
       (close input-port)

       ;; Write out the contents of the file
       (let ((fp (open-input-file tmpl #:binary #t)))
	 (display
	  (let loop ((c (read-char fp))
		     (str ""))
	    (if (not (eof-object? c))
		(loop (read-char fp)
		      (string-append str (string c)))
		;; else
		str))))
       
       (delete-file tmpl)

       (newline)

       (format #t "newterm: ~s~%" scr)
       
       (screen? scr))
     ;; else
     'skipped))

