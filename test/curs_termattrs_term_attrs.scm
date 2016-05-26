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

(automake-test
 (let ((win (initscr)))
   (start-color!)
   (clear win)
   (refresh win) 
   (let ((ta (term-attrs))
	 (ta2 (logior
	       A_ALTCHARSET
	       A_BLINK
	       A_BOLD
	       A_DIM
	       A_COLOR
	       A_INVIS
	       ;; A_PROTECT
	       A_REVERSE
	       A_STANDOUT
	       A_UNDERLINE)))
     (define (yn attrib)
       (string-append
	(if (logtest attrib ta)
	    "yes"
	    " no")
	" "
	(if (logtest attrib ta2)
	    "yes"
	    " no")))
     
     (endwin)
     (newline)
     (format #t "~%")
     (format #t "Terminal Capabilities~%")
     (format #t "this term vs xterm~%")
     (format #t "------------------------------~%")
     (format #t "  PROTECT ~a~%" (yn A_PROTECT))
     (format #t "INVISIBLE ~a~%" (yn A_INVIS))
     (format #t "      ALT ~a~%" (yn A_ALTCHARSET))
     (format #t "     BOLD ~a~%" (yn A_BOLD))
     (format #t "      DIM ~a~%" (yn A_DIM))
     (format #t "    BLINK ~a~%" (yn A_BLINK))
     (format #t "  REVERSE ~a~%" (yn A_REVERSE))
     (format #t "UNDERLINE ~a~%" (yn A_UNDERLINE))
     (format #t " STANDOUT ~a~%" (yn A_STANDOUT))
     (format #t "    COLOR ~a~%" (yn A_COLOR))
     (maybe-sleep 2)
     (and (logtest ta A_ALTCHARSET)
	  (logtest ta A_BLINK)
	  (logtest ta A_BOLD)
	  (logtest ta A_DIM)
	  (logtest ta A_COLOR)
	  (logtest ta A_INVIS)
	  (not (logtest ta A_PROTECT))
	  (logtest ta A_REVERSE)
	  (logtest ta A_STANDOUT)
	  (logtest ta A_UNDERLINE)))))
