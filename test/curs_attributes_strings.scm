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
     (format #t "~a:~% s1: ~s~% s2: ~s~% result: ~s~%" name a b (list= xchar-equal? a b))
     (if (not (list= xchar-equal? a b))
	 (set! pass #f)))
   (endwin)
   (newline)

   (pass-if "(normal \"hi\") == (normal-on \"hi\")"
	    (normal "hi") (normal-on "hi"))

   (pass-if "(normal \"hi\") == (normal-off \"hi\")"
	    (normal "hi") (normal-off "hi"))

   (pass-if "(bold \"hi\") == (bold-on \"hi\")"
	    (bold "hi") (bold-on "hi"))

   (pass-if "(bold-off \"hi\") == (normal \"hi\")"
	    (bold-off "hi") (normal "hi"))

   ;; normal
   (pass-if "(normal (normal \"hi\")) == (normal \"hi\")"
	    (normal (normal "hi")) (normal "hi"))

   (pass-if "(normal (normal-on \"hi\")) == (normal \"hi\")"
	    (normal (normal-on "hi")) (normal "hi"))

   (pass-if "(normal (normal-off \"hi\")) == (normal \"hi\")"
	    (normal (normal-off "hi")) (normal "hi"))

   (pass-if "(normal (bold \"hi\")) == (normal \"hi\")"
	    (normal (bold "hi")) (normal "hi"))

   (pass-if "(normal (bold-on \"hi\")) == (normal \"hi\")"
	    (normal (bold-on "hi")) (normal "hi"))

   (pass-if "(normal (bold-off \"hi\")) == (normal \"hi\")"
	    (normal (bold-off "hi")) (normal "hi"))

   ;; normal-on
   (pass-if "(normal-on (normal \"hi\")) == (normal \"hi\")"
	    (normal-on (normal "hi")) (normal "hi"))

   (pass-if "(normal-on (normal-on \"hi\")) == (normal \"hi\")"
	    (normal-on (normal-on "hi")) (normal "hi"))

   (pass-if "(normal-on (normal-off \"hi\")) == (normal \"hi\")"
	    (normal-on (normal-off "hi")) (normal "hi"))

   (pass-if "(normal-on (bold \"hi\")) == (bold \"hi\")"
	    (normal-on (bold "hi")) (bold "hi"))

   (pass-if "(normal-on (bold-on \"hi\")) == (bold \"hi\")"
	    (normal-on (bold-on "hi")) (bold "hi"))

   (pass-if "(normal-on (bold-off \"hi\")) == (normal \"hi\")"
	    (normal-on (bold-off "hi")) (normal "hi"))

   ;; normal-off
   (pass-if "(normal-off (normal \"hi\")) == (normal \"hi\")"
	    (normal-off (normal "hi")) (normal "hi"))

   (pass-if "(normal-of (normal-on \"hi\")) == (normal \"hi\")"
	    (normal-off (normal-on "hi")) (normal "hi"))

   (pass-if "(normal-off (normal-off \"hi\")) == (normal \"hi\")"
	    (normal-off (normal-off "hi")) (normal "hi"))

   (pass-if "(normal-off (bold \"hi\")) == (bold \"hi\")"
	    (normal-off (bold "hi")) (bold "hi"))

   (pass-if "(normal-off (bold-on \"hi\")) == (bold \"hi\")"
	    (normal-off (bold-on "hi")) (bold "hi"))

   (pass-if "(normal-off (bold-off \"hi\")) == (normal \"hi\")"
	    (normal-off (bold-off "hi")) (normal "hi"))

   ;; bold
   (pass-if "(bold (normal \"hi\")) == (bold \"hi\")"
	    (bold (normal "hi")) (bold "hi"))

   (pass-if "(bold (normal-on \"hi\")) == (bold \"hi\")"
	    (bold (normal-on "hi")) (bold "hi"))

   (pass-if "(bold (normal-off \"hi\")) == (bold \"hi\")"
	    (bold (normal-off "hi")) (bold "hi"))

   (pass-if "(bold (bold \"hi\")) == (bold \"hi\")"
	    (bold (bold "hi")) (bold "hi"))

   (pass-if "(bold (bold-on \"hi\")) == (bold \"hi\")"
	    (bold (bold-on "hi")) (bold "hi"))

   (pass-if "(bold (bold-off \"hi\")) == (bold \"hi\")"
	    (bold (bold-off "hi")) (bold "hi"))

   ;; bold-on
   (pass-if "(bold-on (normal \"hi\")) == (bold \"hi\")"
	    (bold-on (normal "hi")) (bold "hi"))

   (pass-if "(bold-on (normal-on \"hi\")) == (bold \"hi\")"
	    (bold-on (normal-on "hi")) (bold "hi"))

   (pass-if "(bold-on (normal-off \"hi\")) == (bold \"hi\")"
	    (bold-on (normal-off "hi")) (bold "hi"))

   (pass-if "(bold-on (bold \"hi\")) == (bold \"hi\")"
	    (bold-on (bold "hi")) (bold "hi"))

   (pass-if "(bold-on (bold-on \"hi\")) == (bold \"hi\")"
	    (bold-on (bold-on "hi")) (bold "hi"))

   (pass-if "(bold-on (bold-off \"hi\")) == (bold \"hi\")"
	    (bold-on (bold-off "hi")) (bold "hi"))

   ;; bold-off
   (pass-if "(bold-off (normal \"hi\")) == (normal \"hi\")"
	    (bold-off (normal "hi")) (normal "hi"))

   (pass-if "(bold-off (normal-on \"hi\")) == (normal \"hi\")"
	    (bold-off (normal-on "hi")) (normal "hi"))

   (pass-if "(bold-off (normal-off \"hi\")) == (normal \"hi\")"
	    (bold-off (normal-off "hi")) (normal "hi"))

   (pass-if "(bold-off (bold \"hi\")) == (normal \"hi\")"
	    (bold-off (bold "hi")) (normal "hi"))

   (pass-if "(bold-off (bold-on \"hi\")) == (normal \"hi\")"
	    (bold-off (bold-on "hi")) (normal "hi"))

   (pass-if "(bold-off (bold-off \"hi\")) == (normal \"hi\")"
	    (bold-off (bold-off "hi")) (normal "hi"))

   ;; inverse
   (pass-if "(inverse (normal \"hi\")) == (inverse \"hi\")"
	    (inverse (normal "hi")) (inverse "hi"))

   (pass-if "(inverse (normal-on \"hi\")) == (inverse \"hi\")"
	    (inverse (normal-on "hi")) (inverse "hi"))

   (pass-if "(inverse (normal-off \"hi\")) == (inverse \"hi\")"
	    (inverse (normal-off "hi")) (inverse "hi"))

   (pass-if "(inverse (bold \"hi\")) == (inverse \"hi\")"
	    (inverse (bold "hi")) (inverse "hi"))

   (pass-if "(inverse (bold-on \"hi\")) == (inverse \"hi\")"
	    (inverse (bold-on "hi")) (inverse "hi"))

   (pass-if "(inverse (bold-off \"hi\")) == (inverse \"hi\")"
	    (inverse (bold-off "hi")) (inverse "hi"))

   ;; inverse-on
   (pass-if "(inverse-on (normal \"hi\")) == (inverse \"hi\")"
	    (inverse-on (normal "hi")) (inverse "hi"))

   (pass-if "(inverse-on (normal-on \"hi\")) == (inverse \"hi\")"
	    (inverse-on (normal-on "hi")) (inverse "hi"))

   (pass-if "(inverse-on (normal-off \"hi\")) == (inverse \"hi\")"
	    (inverse-on (normal-off "hi")) (inverse "hi"))

   (pass-if "(inverse-on (bold \"hi\")) == (bold-on (inverse \"hi\"))"
	    (inverse-on (bold "hi")) (bold-on (inverse "hi")))

   (pass-if "(inverse-on (bold-on \"hi\")) == (bold-on (inverse \"hi\"))"
	    (inverse-on (bold-on "hi")) (bold-on (inverse "hi")))

   (pass-if "(inverse-on (bold-off \"hi\")) == (inverse \"hi\")"
	    (inverse-on (bold-off "hi")) (inverse "hi"))

   ;; inverse-off
   (pass-if "(inverse-off (normal \"hi\")) == (normal \"hi\")"
	    (inverse-off (normal "hi")) (normal "hi"))

   (pass-if "(inverse-off (normal-on \"hi\")) == (normal \"hi\")"
	    (inverse-off (normal-on "hi")) (normal "hi"))

   (pass-if "(inverse-off (normal-off \"hi\")) == (normal \"hi\")"
	    (inverse-off (normal-off "hi")) (normal "hi"))

   (pass-if "(inverse-off (bold \"hi\")) == (bold \"hi\")"
	    (inverse-off (bold "hi")) (bold "hi"))

   (pass-if "(inverse-off (bold-on \"hi\")) == (bold \"hi\")"
	    (inverse-off (bold-on "hi")) (bold "hi"))

   (pass-if "(inverse-off (bold-off \"hi\")) == (normal \"hi\")"
	    (inverse-off (bold-off "hi")) (normal "hi"))
   pass))

