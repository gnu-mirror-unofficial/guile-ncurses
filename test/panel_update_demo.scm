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
             (ncurses panel))

(automake-test
 (let* ((mainwin (initscr))
	(win1 (newwin 5 10 5 5 #:panel #t))
	(win2 (newwin 5 10 7 7 #:panel #t)))
   (box win1
	(normal (acs-vline))
	(normal (acs-hline)))
   (addstr win1 "box 1"  #:y 1 #:x 1)
   (box win2
	(normal (acs-vline))
	(normal (acs-hline)))
   (addstr win2 "box 2" #:y 1 #:x 1)
   (update-panels)
   (doupdate)
   (maybe-sleep 1)
   
   (addstr win1 "bottom" #:y 1 #:x 1)
   (addstr win2 "box 2 " #:y 1 #:x 1)
   (bottom-panel win1)
   (update-panels)
   (doupdate)
   (maybe-sleep 1)
   
   (addstr win1 "top   " #:y 1 #:x 1)
   (addstr win2 "box 2 " #:y 1 #:x 1)
   (top-panel win1)
   (update-panels)
   (doupdate)
   (maybe-sleep 1)
   
   (addstr win1 "hide   " #:y 1 #:x 1)
   (addstr win2 "box 2  " #:y 1 #:x 1)
   (hide-panel win1)
   (update-panels)
   (doupdate)
   (maybe-sleep 1)
   
   (addstr win1 "show   " #:y 1 #:x 1)
   (addstr win2 "box 2  " #:y 1 #:x 1)
   (show-panel win1)
   (update-panels)
   (doupdate)
   (maybe-sleep 1)
   
   (addstr win1 "move   " #:y 1 #:x 1)
   (addstr win2 "box 2  " #:y 1 #:x 1)
   (move-panel win1 2 2)
   (update-panels)
   (doupdate)
   (maybe-sleep 1)
   
   (addstr win1 "pwin1  " #:y 1 #:x 1)
   (addstr win2 "pwin2  " #:y 1 #:x 1)
   (move-panel win1 2 2)
   (update-panels)
   (doupdate)
   (maybe-sleep 1)
   
   (addstr win1 "delete " #:y 1 #:x 1)
   (addstr win2 "box 2  " #:y 1 #:x 1)
   (del-panel! win1)
   (update-panels)
   (doupdate)
   (maybe-sleep 1)
   
   (endwin)
   
   'skipped))

