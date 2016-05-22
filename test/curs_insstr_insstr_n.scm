(use-modules (test automake-test-lib)
             (ncurses curses)
             (srfi srfi-1))

(setlocale LC_ALL "")

(automake-test
 (let ((win (initscr)))
   (clear win)
   (refresh win)
   (addstr win "xxx" #:y 0 #:x 0)
   ;; Only insert the 1st of these letters 'z'
   (insstr win "zzz" #:y 0 #:x 0 #:n 1)
   (refresh win)
   (let ((x1 (inch win #:y 0 #:x 0))
	 (x2 (inch win #:y 0 #:x 1)))
     (endwin)
     (newline)
     (write x1)
     (newline)
     (write x2)
     (newline)
     (and
      (xchar-equal? x1 (normal #\z))
      (xchar-equal? x2 (normal #\x))))))
