(use-modules (test automake-test-lib)
             (ncurses curses)
             (srfi srfi-1))

(automake-test
 (let ((win (initscr)))
   (clear win)
   (refresh win)
   (halfdelay! 1)
   (flushinp)
   (let* ((c (getch win))
	  (pass (equal? #f c)))
     (endwin)
     (newline)
     (format #t "getch: ~S~%" c)
     (if pass
	 #t
	 ;; else
	 (begin
	   (display "WARNING: this test has failed,  but, this may not
indicate a true problem.  It may report a false failure if you were
generating keypresses or mouse clicks when the test was running.
")
	   'skipped)))))
