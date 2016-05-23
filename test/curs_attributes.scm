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
             (srfi srfi-1))

(automake-test
 (let ((win (initscr))
       (pass #t))
   (define (pass-if name a b)
     (format #t "~a: ~s ~s ~s~%" name a b (xchar-equal? a b))
     (if (not (xchar-equal? a b))
	 (set! pass #f)))
   (endwin)
   (newline)
   
   ;; normal
   (pass-if "(normal #\a) == (normal-on #\a)" (normal #\a) (normal-on #\a))

   (pass-if "(normal #\a) == (normal-off #\a)" (normal #\a) (normal-off #\a))

   ;; bold
   (pass-if "(bold #\a) == (bold-on #\a)" (bold #\a) (bold-on #\a))

   (pass-if "(bold-off #\a) == (normal #\a)" (bold-off #\a) (normal #\a))

   ;; normal
   (pass-if "(normal (normal #\a)) == (normal #\a)"
	    (normal (normal #\a)) (normal #\a))

   (pass-if "(normal (normal-on #\a)) == (normal #\a)"
	    (normal (normal-on #\a)) (normal #\a))
   (pass-if "(normal (normal-off #\a)) == (normal #\a)"
	    (normal (normal-off #\a)) (normal #\a))
   (pass-if "(normal (bold #\a)) == (normal #\a)"
	    (normal (bold #\a)) (normal #\a))
   (pass-if "(normal (bold-on #\a)) == (normal #\a)"
	    (normal (bold-on #\a)) (normal #\a))
   (pass-if "(normal (bold-off #\a)) == (normal #\a)"
	    (normal (bold-off #\a)) (normal #\a))

   ;; normal-on
   (pass-if "(normal-on (normal #\a)) == (normal #\a)"
	    (normal-on (normal #\a)) (normal #\a))

   (pass-if "(normal-on (normal-on #\a)) == (normal #\a)"
	    (normal-on (normal-on #\a)) (normal #\a))

   (pass-if "(normal-on (normal-off #\a)) == (normal #\a)"
	    (normal-on (normal-off #\a)) (normal #\a))

   (pass-if "(normal-on (bold #\a)) == (bold #\a)"
	    (normal-on (bold #\a)) (bold #\a))

   (pass-if "(normal-on (bold-on #\a)) == (bold #\a)"
	    (normal-on (bold-on #\a)) (bold #\a))

   (pass-if "(normal-on (bold-off #\a)) == (normal #\a)"
	    (normal-on (bold-off #\a)) (normal #\a))

   ;; normal-off
   (pass-if "(normal-off (normal #\a)) == (normal #\a)"
	    (normal-off (normal #\a)) (normal #\a))

   (pass-if "(normal-of (normal-on #\a)) == (normal #\a)"
	    (normal-off (normal-on #\a)) (normal #\a))

   (pass-if "(normal-off (normal-off #\a)) == (normal #\a)"
	    (normal-off (normal-off #\a)) (normal #\a))

   (pass-if "(normal-off (bold #\a)) == (bold #\a)"
	    (normal-off (bold #\a)) (bold #\a))

   (pass-if "(normal-off (bold-on #\a)) == (bold #\a)"
	    (normal-off (bold-on #\a)) (bold #\a))

   (pass-if "(normal-off (bold-off #\a)) == (normal #\a)"
	    (normal-off (bold-off #\a)) (normal #\a))

   ;; bold
   (pass-if "(bold (normal #\a)) == (bold #\a)"
	    (bold (normal #\a)) (bold #\a))

   (pass-if "(bold (normal-on #\a)) == (bold #\a)"
	    (bold (normal-on #\a)) (bold #\a))

   (pass-if "(bold (normal-off #\a)) == (bold #\a)"
	    (bold (normal-off #\a)) (bold #\a))

   (pass-if "(bold (bold #\a)) == (bold #\a)"
	    (bold (bold #\a)) (bold #\a))

   (pass-if "(bold (bold-on #\a)) == (bold #\a)"
	    (bold (bold-on #\a)) (bold #\a))

   (pass-if "(bold (bold-off #\a)) == (bold #\a)"
	    (bold (bold-off #\a)) (bold #\a))

   ;; bold-on
   (pass-if "(bold-on (normal #\a)) == (bold #\a)"
	    (bold-on (normal #\a)) (bold #\a))

   (pass-if "(bold-on (normal-on #\a)) == (bold #\a)"
	    (bold-on (normal-on #\a)) (bold #\a))

   (pass-if "(bold-on (normal-off #\a)) == (bold #\a)"
	    (bold-on (normal-off #\a)) (bold #\a))

   (pass-if "(bold-on (bold #\a)) == (bold #\a)"
	    (bold-on (bold #\a)) (bold #\a))

   (pass-if "(bold-on (bold-on #\a)) == (bold #\a)"
	    (bold-on (bold-on #\a)) (bold #\a))

   (pass-if "(bold-on (bold-off #\a)) == (bold #\a)"
	    (bold-on (bold-off #\a)) (bold #\a))

   ;; bold-off
   (pass-if "(bold-off (normal #\a)) == (normal #\a)"
	    (bold-off (normal #\a)) (normal #\a))

   (pass-if "(bold-off (normal-on #\a)) == (normal #\a)"
	    (bold-off (normal-on #\a)) (normal #\a))

   (pass-if "(bold-off (normal-off #\a)) == (normal #\a)"
	    (bold-off (normal-off #\a)) (normal #\a))

   (pass-if "(bold-off (bold #\a)) == (normal #\a)"
	    (bold-off (bold #\a)) (normal #\a))

   (pass-if "(bold-off (bold-on #\a)) == (normal #\a)"
	    (bold-off (bold-on #\a)) (normal #\a))

   (pass-if "(bold-off (bold-off #\a)) == (normal #\a)"
	    (bold-off (bold-off #\a)) (normal #\a))

   ;; inverse
   (pass-if "(inverse (normal #\a)) == (inverse #\a)"
	    (inverse (normal #\a)) (inverse #\a))

   (pass-if "(inverse (normal-on #\a)) == (inverse #\a)"
	    (inverse (normal-on #\a)) (inverse #\a))

   (pass-if "(inverse (normal-off #\a)) == (inverse #\a)"
	    (inverse (normal-off #\a)) (inverse #\a))

   (pass-if "(inverse (bold #\a)) == (inverse #\a)"
	    (inverse (bold #\a)) (inverse #\a))

   (pass-if "(inverse (bold-on #\a)) == (inverse #\a)"
	    (inverse (bold-on #\a)) (inverse #\a))

   (pass-if "(inverse (bold-off #\a)) == (inverse #\a)"
	    (inverse (bold-off #\a)) (inverse #\a))

   ;; inverse-on
   (pass-if "(inverse-on (normal #\a)) == (inverse #\a)"
	    (inverse-on (normal #\a)) (inverse #\a))

   (pass-if "(inverse-on (normal-on #\a)) == (inverse #\a)"
	    (inverse-on (normal-on #\a)) (inverse #\a))

   (pass-if "(inverse-on (normal-off #\a)) == (inverse #\a)"
	    (inverse-on (normal-off #\a)) (inverse #\a))

   (pass-if "(inverse-on (bold #\a)) == (bold-on (inverse #\a))"
	    (inverse-on (bold #\a)) (bold-on (inverse #\a)))

   (pass-if "(inverse-on (bold-on #\a)) == (bold-on (inverse #\a))"
	    (inverse-on (bold-on #\a)) (bold-on (inverse #\a)))

   (pass-if "(inverse-on (bold-off #\a)) == (inverse #\a)"
	    (inverse-on (bold-off #\a)) (inverse #\a))

   ;; inverse-off
   (pass-if "(inverse-off (normal #\a)) == (normal #\a)"
	    (inverse-off (normal #\a)) (normal #\a))

   (pass-if "(inverse-off (normal-on #\a)) == (normal #\a)"
	    (inverse-off (normal-on #\a)) (normal #\a))

   (pass-if "(inverse-off (normal-off #\a)) == (normal #\a)"
	    (inverse-off (normal-off #\a)) (normal #\a))

   (pass-if "(inverse-off (bold #\a)) == (bold #\a)"
	    (inverse-off (bold #\a)) (bold #\a))

   (pass-if "(inverse-off (bold-on #\a)) == (bold #\a)"
	    (inverse-off (bold-on #\a)) (bold #\a))

   (pass-if "(inverse-off (bold-off #\a)) == (normal #\a)"
	    (inverse-off (bold-off #\a)) (normal #\a))
   pass))

