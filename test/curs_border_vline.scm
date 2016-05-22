(use-modules (test automake-test-lib)
             (ncurses curses))

(automake-test
 (let ((win (initscr)))
   (clear win)
   (move win 1 1)
   (vline win (normal #\!) 2)
   (refresh win)
   (maybe-sleep 2)
   (let ((x1  (inch win #:y 1 #:x 1))
	 (x2  (inch win #:y 2 #:x 1)))
     (endwin)
     (newline)
     (format #t "at (1,1) ~s~%" x1)
     (format #t "at (1,2) ~s~%" x2)
     (and
      (xchar-equal? x1 (normal #\!))
      (xchar-equal? x2 (normal #\!))))))
