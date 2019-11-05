;; Copyright 2009, 2010, 2016, 2019 Free Software Foundation, Inc.

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

;; Here we open a pair of terminal screen that read and write to a files
;; instead of to the screen.  Normally you'd only be doing this on
;; /dev/ttyS1 or whatever, but, not for this test.

;; It should be possible to set output to the two different screens.

;; The two newterm procedures should each return a #<screen>

;; Write out the contents of the file
(define (write-binary-file fname)
  (let ((fp (open-input-file fname #:binary #t)))
    (display
     (let loop ((c (read-char fp))
                (str ""))
       (if (not (eof-object? c))
           (loop (read-char fp)
                 (string-append str (string c)))
           ;; else
           str)))))

(automake-test
 (if (defined? 'newterm)
     (let* ((tmpl-1 (string-copy "gucuXXXXXX"))
            (tmpl-2 (string-copy "gucuXXXXXX"))
            (input-port-1 (mkstemp! tmpl-1))
            (input-port-2 (mkstemp! tmpl-2))
            (output-port-1 input-port-1)
            (output-port-2 input-port-2)
            (scr-1 (newterm "xterm" output-port-1 input-port-1))
            (scr-2 (newterm "vt100" output-port-2 input-port-2)))
       (set-term scr-1)
       (addstr (stdscr) "window #1!!")
       (refresh (stdscr))

       (set-term scr-2)
       (addstr (stdscr) "window #2!!")
       (refresh (stdscr))

       (endwin)
       
       (close input-port-1)
       (close input-port-2)

       (newline)

       (format #t "Screen 1 ~s:~%" tmpl-1)
       (write-binary-file tmpl-1)
       (newline)

       (format #t "Screen 2 ~s:~%" tmpl-2)
       (write-binary-file tmpl-2)
       (newline)
       
       (delete-file tmpl-1)
       (delete-file tmpl-2)

       (format #t "newterm #1: ~s~%" scr-1)
       (format #t "newterm #2: ~s~%" scr-2)

       (and
        (screen? scr-1)
        (screen? scr-2)
        (not (screen=? scr-1 scr-2))))
     ;; else
     'skipped))
