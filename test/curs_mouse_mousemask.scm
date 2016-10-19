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
             (ncurses curses)
	     (ice-9 format)
             (srfi srfi-1))

;; xterm can do mouse events

(automake-test
 (if (not (defined? 'has-mouse?))
     'skipped
     (let ((win (initscr)))
       (clear win)
       (refresh win)
       (let ((mflag    (mousemask ALL_MOUSE_EVENTS)))
	 (endwin)
	 (newline)
	 (if (has-mouse?)
	     (begin
	       (format #t "mousemask: ~x~%" mflag)
	       (if (logtest BUTTON_SHIFT mflag)
		   (format #t "shift \n"))
	       (if (logtest BUTTON_CTRL mflag)
		   (format #t "ctrl \n"))
	       (if (logtest BUTTON_ALT mflag)
		   (format #t "alt \n"))
	       (if (logtest BUTTON1_PRESSED mflag)
		   (format #t "button 1 pressed\n"))
	       (if (logtest BUTTON1_RELEASED mflag)
		   (format #t "button 1 released\n"))
	       (if (logtest BUTTON1_CLICKED mflag)
		   (format #t "button 1 clicked\n"))
	       (if (logtest BUTTON1_DOUBLE_CLICKED mflag)
		   (format #t "button 1 double clicked\n"))
	       (if (logtest BUTTON1_TRIPLE_CLICKED mflag)
		   (format #t "button 1 triple clicked\n"))
	       (if (logtest BUTTON2_PRESSED mflag)
		   (format #t "button 2 pressed\n"))
	       (if (logtest BUTTON2_RELEASED mflag)
		   (format #t "button 2 released\n"))
	       (if (logtest BUTTON2_CLICKED mflag)
		   (format #t "button 2 clicked\n"))
	       (if (logtest BUTTON2_DOUBLE_CLICKED mflag)
		   (format #t "button 2 double clicked\n"))
	       (if (logtest BUTTON2_TRIPLE_CLICKED mflag)
		   (format #t "button 2 triple clicked\n"))
	       (if (logtest BUTTON3_PRESSED mflag)
		   (format #t "button 3 pressed\n"))
	       (if (logtest BUTTON3_RELEASED mflag)
		   (format #t "button 3 released\n"))
	       (if (logtest BUTTON3_CLICKED mflag)
		   (format #t "button 3 clicked\n"))
	       (if (logtest BUTTON3_DOUBLE_CLICKED mflag)
		   (format #t "button 3 double clicked\n"))
	       (if (logtest BUTTON3_TRIPLE_CLICKED mflag)
		   (format #t "button 3 triple clicked\n"))
	       (if (logtest BUTTON4_PRESSED mflag)
		   (format #t "button 4 pressed\n"))
	       (if (logtest BUTTON4_RELEASED mflag)
		   (format #t "button 4 released\n"))
	       (if (logtest BUTTON4_CLICKED mflag)
		   (format #t "button 4 clicked\n"))
	       (if (logtest BUTTON4_DOUBLE_CLICKED mflag)
		   (format #t "button 4 double clicked\n"))
	       (if (logtest BUTTON4_TRIPLE_CLICKED mflag)
		   (format #t "button 4 triple clicked\n"))
	       ;; (if (logtest BUTTON5_PRESSED mflag)
	       ;;     (format #t "button 5 pressed\n"))
	       ;; (if (logtest BUTTON5_RELEASED mflag)
	       ;;     (format #t "button 5 released\n"))
	       ;; (if (logtest BUTTON5_CLICKED mflag)
	       ;;     (format #t "button 5 clicked\n"))
	       ;; (if (logtest BUTTON5_DOUBLE_CLICKED mflag)
	       ;;     (format #t "button 5 double clicked\n"))
	       ;; (if (logtest BUTTON5_TRIPLE_CLICKED mflag)
	       ;;     (format #t "button 5 triple clicked\n"))
	       (and (logtest mflag BUTTON_SHIFT)
		    (logtest mflag BUTTON_CTRL)
		    (logtest mflag BUTTON_ALT)

		    (logtest mflag BUTTON1_PRESSED)
		    (logtest mflag BUTTON1_RELEASED)
		    (logtest mflag BUTTON1_CLICKED)
		    (logtest mflag BUTTON1_DOUBLE_CLICKED)

		    (logtest mflag BUTTON2_PRESSED)
		    (logtest mflag BUTTON2_RELEASED)
		    (logtest mflag BUTTON2_CLICKED)
		    (logtest mflag BUTTON2_DOUBLE_CLICKED)

		    (logtest mflag BUTTON3_PRESSED)
		    (logtest mflag BUTTON3_RELEASED)
		    (logtest mflag BUTTON3_CLICKED)
		    (logtest mflag BUTTON3_DOUBLE_CLICKED)

		    (logtest mflag BUTTON4_PRESSED)
		    (logtest mflag BUTTON4_RELEASED)
		    (logtest mflag BUTTON4_CLICKED)
		    (logtest mflag BUTTON4_DOUBLE_CLICKED)))

	     ;; else
	     (begin
	       (format #t "mousemask: NONE~%")
	       #f))))))
